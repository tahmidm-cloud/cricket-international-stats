import sys
import json
from pathlib import Path

import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

import fetch_espn_everything_possible as espn


OUT_DIR = Path("outputs")

PROFILES_CSV = OUT_DIR / "espn_everything_profiles.csv"
PROFILES_JSON = OUT_DIR / "espn_everything_profiles.json"
ERRORS_CSV = OUT_DIR / "espn_everything_errors.csv"
TEAM_REF_CSV = OUT_DIR / "espn_team_reference.csv"
REPORT_CSV = OUT_DIR / "espn_everything_report.csv"

PLAYER_INDEX_ENRICHED = OUT_DIR / "player_index_enriched.csv"
PLAYER_INDEX_BASE = OUT_DIR / "player_index.csv"

CORE_CACHE_DIR = OUT_DIR / "espn_core_cache"


def normalize_id(x):
    return str(x).replace(".0", "").strip()


def load_player_index():
    path = PLAYER_INDEX_ENRICHED if PLAYER_INDEX_ENRICHED.exists() else PLAYER_INDEX_BASE
    df = pd.read_csv(path, dtype=str).fillna("")
    df["cricinfo_id"] = df["cricinfo_id"].map(normalize_id)

    df = df[
        (df["cricinfo_id"] != "")
        & (~df["cricinfo_id"].str.startswith("missing_", na=False))
    ].copy()

    df = df.drop_duplicates("cricinfo_id").reset_index(drop=True)
    return df


def load_team_rows():
    if not TEAM_REF_CSV.exists():
        return {}

    teams = pd.read_csv(TEAM_REF_CSV, dtype=str).fillna("")
    if "team_id" not in teams.columns:
        return {}

    teams["team_id"] = teams["team_id"].map(normalize_id)
    return {
        row["team_id"]: row.to_dict()
        for _, row in teams.iterrows()
        if row["team_id"]
    }


def save_profiles_json(profiles_df):
    profiles_json = {}

    for _, row in profiles_df.iterrows():
        pid = normalize_id(row.get("cricinfo_id", ""))
        if not pid:
            continue

        profiles_json[pid] = {
            k: (None if pd.isna(v) else v)
            for k, v in row.to_dict().items()
        }

    PROFILES_JSON.write_text(
        json.dumps(profiles_json, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )


def main():
    if not PROFILES_CSV.exists():
        raise FileNotFoundError(f"Missing {PROFILES_CSV}")

    if not ERRORS_CSV.exists():
        raise FileNotFoundError(f"Missing {ERRORS_CSV}")

    profiles = pd.read_csv(PROFILES_CSV, dtype=str).fillna("")
    errors = pd.read_csv(ERRORS_CSV, dtype=str).fillna("")

    if errors.empty:
        print("No errors to retry.")
        return

    if "cricinfo_id" not in errors.columns:
        raise ValueError("errors file missing cricinfo_id column")

    players = load_player_index()
    player_by_id = {
        row["cricinfo_id"]: row
        for _, row in players.iterrows()
    }

    team_rows_by_id = load_team_rows()

    retry_ids = list(dict.fromkeys(errors["cricinfo_id"].map(normalize_id).tolist()))

    print("Existing profiles:", len(profiles))
    print("Errors to retry:", len(retry_ids))

    successful_rows = []
    remaining_errors = []

    for i, pid in enumerate(retry_ids, start=1):
        source_row = player_by_id.get(pid)

        if source_row is None:
            remaining_errors.append({
                "cricinfo_id": pid,
                "final_player_name": "",
                "stage": "retry_lookup",
                "status": "missing_from_player_index",
                "error_type": "missing_source_row",
                "preview": "Could not find this cricinfo_id in player index.",
            })
            continue

        player_name = espn.clean_text(source_row.get("final_player_name", ""))

        print(f"[{i}/{len(retry_ids)}] retry {pid} {player_name}")

        # Important: remove cached failed JSON before retrying.
        cache_path = CORE_CACHE_DIR / f"{pid}.json"
        if cache_path.exists():
            cache_path.unlink()

        athlete_data, athlete_status = espn.fetch_athlete(pid)

        if not isinstance(athlete_data, dict) or athlete_data.get("_error"):
            remaining_errors.append({
                "cricinfo_id": pid,
                "final_player_name": player_name,
                "stage": "athlete_core_retry",
                "status": athlete_status,
                "error_type": espn.clean_text(
                    athlete_data.get("error_type") if isinstance(athlete_data, dict) else ""
                ),
                "preview": espn.clean_text(
                    athlete_data.get("text_preview") if isinstance(athlete_data, dict) else ""
                ),
            })
            continue

        try:
            row = espn.parse_athlete(
                athlete_data,
                source_row,
                team_rows_by_id,
                home_probe=None,
            )
            row["athlete_fetch_status"] = athlete_status
            successful_rows.append(row)
        except Exception as e:
            remaining_errors.append({
                "cricinfo_id": pid,
                "final_player_name": player_name,
                "stage": "parse_athlete_retry",
                "status": "exception",
                "error_type": type(e).__name__,
                "preview": str(e),
            })

    print("\nRetry successful:", len(successful_rows))
    print("Still failed:", len(remaining_errors))

    if successful_rows:
        retry_df = pd.DataFrame(successful_rows)

        combined = pd.concat([profiles, retry_df], ignore_index=True, sort=False)
        combined["cricinfo_id"] = combined["cricinfo_id"].map(normalize_id)

        # Keep newest successful row if an ID already exists.
        combined = combined.drop_duplicates("cricinfo_id", keep="last").reset_index(drop=True)

        combined.to_csv(PROFILES_CSV, index=False)
        save_profiles_json(combined)

        print("Updated:", PROFILES_CSV)
        print("Updated:", PROFILES_JSON)
    else:
        combined = profiles

    remaining_errors_df = pd.DataFrame(remaining_errors)
    remaining_errors_df.to_csv(ERRORS_CSV, index=False)

    teams_df = pd.DataFrame(list(team_rows_by_id.values()))
    teams_df.to_csv(TEAM_REF_CSV, index=False)

    report = pd.DataFrame([
        {"item": "players_requested", "value": len(player_by_id)},
        {"item": "profiles_saved", "value": len(combined)},
        {"item": "errors", "value": len(remaining_errors_df)},
        {"item": "teams_resolved", "value": len(teams_df)},
        {"item": "home_probe_rows", "value": 0},
        {"item": "fetch_home_enabled", "value": False},
        {"item": "resolve_teams_enabled", "value": espn.RESOLVE_TEAMS},
        {"item": "sleep_seconds", "value": espn.SLEEP_SECONDS},
        {"item": "retry_successful", "value": len(successful_rows)},
        {"item": "retry_still_failed", "value": len(remaining_errors_df)},
    ])

    report.to_csv(REPORT_CSV, index=False)

    print("\nReport:")
    print(report.to_string(index=False))


if __name__ == "__main__":
    main()