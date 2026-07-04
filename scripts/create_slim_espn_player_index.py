import json
from pathlib import Path

import pandas as pd


PLAYER_INDEX = Path("outputs/player_index_enriched.csv")
FALLBACK_PLAYER_INDEX = Path("outputs/player_index.csv")
ESPN_FINAL = Path("outputs/espn_profiles_styles_final.csv")

OUT_CSV = Path("outputs/player_index_espn_slim.csv")
OUT_JSON_LIST = Path("outputs/player_index_espn_slim.json")
OUT_JSON_KEYED = Path("outputs/player_index_espn_slim_keyed.json")
OUT_REPORT = Path("outputs/player_index_espn_slim_report.csv")
OUT_MISSING = Path("outputs/player_index_espn_slim_missing.csv")


def clean_value(x):
    if pd.isna(x):
        return ""

    x = str(x).strip()

    if x.lower() in {"nan", "none", "null"}:
        return ""

    return x


def clean_id(x):
    x = clean_value(x)
    return x.replace(".0", "").strip()


def clean_date(x):
    x = clean_value(x)

    if not x:
        return ""

    # ESPN usually gives: 1998-12-07T00:00Z
    # Keep only: 1998-12-07
    if "T" in x:
        return x.split("T")[0]

    return x[:10]


def clean_bool(x):
    x = clean_value(x).lower()
    return x in {"true", "1", "yes"}


def first_existing(row, cols):
    for col in cols:
        if col in row.index:
            val = clean_value(row.get(col, ""))
            if val:
                return val

    return ""


def active_status_from_row(row, espn_found):
    if not espn_found:
        return "", ""

    raw = first_existing(row, ["is_active", "is_active_espn", "active", "active_espn"])

    if raw == "":
        return "", ""

    is_active = clean_bool(raw)
    active_status = "active" if is_active else "inactive"

    return is_active, active_status


def main():
    index_path = PLAYER_INDEX if PLAYER_INDEX.exists() else FALLBACK_PLAYER_INDEX

    if not index_path.exists():
        raise FileNotFoundError("Missing outputs/player_index_enriched.csv or outputs/player_index.csv")

    if not ESPN_FINAL.exists():
        raise FileNotFoundError("Missing outputs/espn_profiles_styles_final.csv")

    print("Using player index:", index_path)
    print("Using ESPN final:", ESPN_FINAL)

    idx = pd.read_csv(index_path, dtype=str).fillna("")
    espn = pd.read_csv(ESPN_FINAL, dtype=str).fillna("")

    if "cricinfo_id" not in idx.columns:
        raise ValueError("Player index missing cricinfo_id")

    if "cricinfo_id" not in espn.columns:
        raise ValueError("ESPN final missing cricinfo_id")

    idx["cricinfo_id"] = idx["cricinfo_id"].map(clean_id)
    espn["cricinfo_id"] = espn["cricinfo_id"].map(clean_id)

    # ESPN file should be one row per successful profile.
    espn = espn.drop_duplicates("cricinfo_id", keep="last").copy()

    merged = idx.merge(
        espn,
        on="cricinfo_id",
        how="left",
        suffixes=("", "_espn"),
        indicator=True,
    )

    rows = []

    for _, row in merged.iterrows():
        espn_found = row["_merge"] == "both"
        espn_is_active, espn_active_status = active_status_from_row(row, espn_found)

        obj = {
            "cricinfo_id": clean_id(row.get("cricinfo_id", "")),
            "unique_player_id": clean_id(first_existing(row, ["unique_player_id", "your_unique_player_id"])),
            "final_player_name": first_existing(row, ["final_player_name", "your_final_player_name"]),
            "source_country_text": first_existing(row, ["source_country_text", "your_country_text"]),

            "espn_profile_found": bool(espn_found),

            "espn_full_name": clean_value(row.get("espn_full_name", "")),
            "espn_display_name": clean_value(row.get("espn_display_name", "")),
            "espn_first_name": clean_value(row.get("espn_first_name", "")),
            "espn_last_name": clean_value(row.get("espn_last_name", "")),
            "espn_date_of_birth": clean_date(row.get("date_of_birth", "")),

            "espn_is_active": espn_is_active,
            "espn_active_status": espn_active_status,

            "standard_batting_code": clean_value(row.get("standard_batting_code", "")),
            "standard_batting_style": clean_value(row.get("standard_batting_style", "")),

            "standard_bowling_code": clean_value(row.get("standard_bowling_code", "")),
            "standard_bowling_style": clean_value(row.get("standard_bowling_style", "")),

            "espn_playing_role": clean_value(row.get("playing_role", "")),
            "espn_playing_role_id": clean_value(row.get("playing_role_id", "")),

            "espn_country_name": clean_value(row.get("country_name", "")),

            "espn_major_teams": clean_value(row.get("major_teams", "")),
        }

        rows.append(obj)

    slim = pd.DataFrame(rows)

    # Preserve the full player-index row structure.
    # If your player index has 8250 rows, this CSV/list JSON keeps 8250 rows.
    slim.to_csv(OUT_CSV, index=False)

    OUT_JSON_LIST.write_text(
        json.dumps(slim.to_dict(orient="records"), indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    # Keyed JSON: one row per cricinfo_id.
    # This is better for the game engine.
    keyed = {}

    for _, row in slim.drop_duplicates("cricinfo_id", keep="first").iterrows():
        pid = row["cricinfo_id"]

        if not pid:
            continue

        keyed[pid] = {
            "cricinfo_id": row["cricinfo_id"],
            "unique_player_id": row["unique_player_id"],
            "final_player_name": row["final_player_name"],
            "source_country_text": row["source_country_text"],

            "espn_profile_found": row["espn_profile_found"],

            "espn_full_name": row["espn_full_name"],
            "espn_display_name": row["espn_display_name"],
            "espn_first_name": row["espn_first_name"],
            "espn_last_name": row["espn_last_name"],
            "espn_date_of_birth": row["espn_date_of_birth"],

            "espn_is_active": row["espn_is_active"],
            "espn_active_status": row["espn_active_status"],

            "standard_batting_code": row["standard_batting_code"],
            "standard_batting_style": row["standard_batting_style"],

            "standard_bowling_code": row["standard_bowling_code"],
            "standard_bowling_style": row["standard_bowling_style"],

            "espn_playing_role": row["espn_playing_role"],
            "espn_playing_role_id": row["espn_playing_role_id"],

            "espn_country_name": row["espn_country_name"],

            "espn_major_teams": row["espn_major_teams"],
        }

    OUT_JSON_KEYED.write_text(
        json.dumps(keyed, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    missing = slim[~slim["espn_profile_found"]].copy()
    missing.to_csv(OUT_MISSING, index=False)

    report = pd.DataFrame([
        {"item": "player_index_rows", "value": len(idx)},
        {"item": "player_index_unique_cricinfo_ids", "value": idx["cricinfo_id"].nunique()},
        {"item": "espn_profiles_rows", "value": len(espn)},
        {"item": "slim_rows", "value": len(slim)},
        {"item": "slim_unique_cricinfo_ids", "value": slim["cricinfo_id"].nunique()},
        {"item": "rows_with_espn_profile", "value": int(slim["espn_profile_found"].sum())},
        {"item": "rows_missing_espn_profile", "value": int((~slim["espn_profile_found"]).sum())},
        {"item": "unique_ids_missing_espn_profile", "value": missing["cricinfo_id"].nunique()},
        {"item": "active_rows", "value": int((slim["espn_active_status"] == "active").sum())},
        {"item": "inactive_rows", "value": int((slim["espn_active_status"] == "inactive").sum())},
        {"item": "blank_active_status_rows", "value": int((slim["espn_active_status"] == "").sum())},
        {"item": "blank_batting_style_rows", "value": int((slim["standard_batting_style"] == "").sum())},
        {"item": "blank_bowling_style_rows", "value": int((slim["standard_bowling_style"] == "").sum())},
    ])

    report.to_csv(OUT_REPORT, index=False)

    print("\nSaved:")
    print(OUT_CSV)
    print(OUT_JSON_LIST)
    print(OUT_JSON_KEYED)
    print(OUT_REPORT)
    print(OUT_MISSING)

    print("\nReport:")
    print(report.to_string(index=False))

    print("\nActive status counts:")
    print(slim["espn_active_status"].replace("", "(blank)").value_counts().to_string())

    print("\nExample:")
    good = slim[slim["espn_profile_found"]].iloc[0].to_dict()
    print(json.dumps(good, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()