import json
from pathlib import Path

import pandas as pd


ARTIFACTS_DIR = Path("artifacts")
OUT_DIR = Path("outputs")
OUT_DIR.mkdir(parents=True, exist_ok=True)

FINAL_CSV = OUT_DIR / "bio_t20s_only.csv"
FINAL_JSON = OUT_DIR / "bio_t20s_only_keyed.json"
FINAL_ERRORS = OUT_DIR / "bio_t20s_only_errors.csv"
FINAL_REPORT = OUT_DIR / "bio_t20s_only_report.csv"


def safe_read_csv(path):
    try:
        if path.stat().st_size == 0:
            return pd.DataFrame()
        return pd.read_csv(path, dtype=str).fillna("")
    except Exception as exc:
        print(f"Skipped CSV {path}: {exc}")
        return pd.DataFrame()


def find_main_shard_csvs():
    files = sorted(ARTIFACTS_DIR.rglob("bio_t20s_only_shard_*_of_*.csv"))

    main_files = [
        f for f in files
        if not f.name.endswith("_errors.csv")
        and not f.name.endswith("_report.csv")
    ]

    return main_files


def merge_main_csvs():
    files = find_main_shard_csvs()
    frames = []

    print(f"Found main shard CSVs: {len(files)}")

    for file in files:
        df = safe_read_csv(file)

        if len(df):
            frames.append(df)
            print(f"Loaded main shard: {file} rows={len(df)}")
        else:
            print(f"Empty/skipped main shard: {file}")

    if not frames:
        return pd.DataFrame()

    out = pd.concat(frames, ignore_index=True)

    if "cricinfo_id" in out.columns:
        out = out.drop_duplicates("cricinfo_id", keep="first")

    sort_cols = [
        col for col in ["final_country_name", "final_full_name", "cricinfo_id"]
        if col in out.columns
    ]

    if sort_cols:
        out = out.sort_values(sort_cols, na_position="last")

    return out


def merge_keyed_jsons():
    files = sorted(ARTIFACTS_DIR.rglob("bio_t20s_only_shard_*_of_*_keyed.json"))
    combined = {}

    print(f"Found keyed JSON shards: {len(files)}")

    for file in files:
        try:
            text = file.read_text(encoding="utf-8").strip()

            if not text:
                print(f"Empty JSON skipped: {file}")
                continue

            data = json.loads(text)

            if isinstance(data, dict):
                combined.update(data)
                print(f"Loaded JSON shard: {file} players={len(data)}")
            else:
                print(f"Skipped non-dict JSON: {file}")

        except Exception as exc:
            print(f"Skipped JSON {file}: {exc}")

    return combined


def merge_error_csvs():
    files = sorted(ARTIFACTS_DIR.rglob("bio_t20s_only_shard_*_of_*_errors.csv"))
    frames = []

    print(f"Found error CSVs: {len(files)}")

    for file in files:
        df = safe_read_csv(file)

        if len(df):
            frames.append(df)
            print(f"Loaded errors: {file} rows={len(df)}")

    if not frames:
        return pd.DataFrame(columns=["cricinfo_id", "input_name", "error"])

    return pd.concat(frames, ignore_index=True)


def build_report(main_df, keyed, errors_df):
    rows = [
        {"item": "final_csv_rows", "value": len(main_df)},
        {"item": "final_json_players", "value": len(keyed)},
        {"item": "final_error_rows", "value": len(errors_df)},
    ]

    if len(main_df):
        if "final_player_status" in main_df.columns:
            for status in ["active", "retired", "passed_away", "unknown"]:
                rows.append({
                    "item": f"status_{status}",
                    "value": int((main_df["final_player_status"] == status).sum())
                })

        check_cols = [
            "final_date_of_birth",
            "final_date_of_death",
            "t20s_bat_matches",
            "t20s_bat_runs",
            "t20s_bat_avg",
            "t20s_bat_sr",
            "t20s_bowl_matches",
            "t20s_bowl_wkts",
            "t20s_bowl_avg",
            "t20s_bowl_econ",
            "t20s_field_catches",
        ]

        for col in check_cols:
            if col in main_df.columns:
                rows.append({
                    "item": f"with_{col}",
                    "value": int((main_df[col].astype(str).str.strip() != "").sum())
                })

    return pd.DataFrame(rows)


def main():
    if not ARTIFACTS_DIR.exists():
        raise FileNotFoundError(
            "Missing artifacts directory. The workflow must download shard artifacts before merging."
        )

    main_df = merge_main_csvs()
    keyed = merge_keyed_jsons()
    errors_df = merge_error_csvs()

    main_df.to_csv(FINAL_CSV, index=False)

    FINAL_JSON.write_text(
        json.dumps(keyed, indent=2, ensure_ascii=False),
        encoding="utf-8"
    )

    errors_df.to_csv(FINAL_ERRORS, index=False)

    report_df = build_report(main_df, keyed, errors_df)
    report_df.to_csv(FINAL_REPORT, index=False)

    print("\nSaved final outputs:")
    print(FINAL_CSV)
    print(FINAL_JSON)
    print(FINAL_ERRORS)
    print(FINAL_REPORT)

    print("\nReport:")
    print(report_df.to_string(index=False))

    if len(main_df):
        print("\nSample:")
        sample_cols = [
            "cricinfo_id",
            "input_name",
            "final_full_name",
            "final_player_status",
            "status_source",
            "final_date_of_birth",
            "final_date_of_death",
            "last_played_year",
            "t20s_bat_matches",
            "t20s_bat_runs",
            "t20s_bat_avg",
            "t20s_bat_sr",
            "t20s_bowl_matches",
            "t20s_bowl_wkts",
            "t20s_bowl_avg",
            "t20s_bowl_econ",
        ]

        sample_cols = [c for c in sample_cols if c in main_df.columns]
        print(main_df[sample_cols].head(25).to_string(index=False))


if __name__ == "__main__":
    main()