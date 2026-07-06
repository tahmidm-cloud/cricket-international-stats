import os
import pandas as pd
from pathlib import Path

INPUT_CSV = Path(os.getenv("INPUT_CSV", "outputs/bio_t20s_more_filled.csv"))
OUT_DIR = Path("outputs")
OUT_DIR.mkdir(parents=True, exist_ok=True)

REVIEW_CSV = OUT_DIR / "bio_t20s_manual_review_needed.csv"
BIO_REVIEW_CSV = OUT_DIR / "bio_t20s_bio_errors_and_dob_review.csv"
STYLE_REVIEW_CSV = OUT_DIR / "bio_t20s_style_review_needed.csv"
REPORT_CSV = OUT_DIR / "bio_t20s_manual_review_report.csv"

EDITABLE_FIELDS = [
    "final_date_of_birth",
    "final_date_of_death",
    "final_player_status",
    "standard_batting_code",
    "standard_batting_style",
    "standard_bowling_code",
    "standard_bowling_style",
    "playing_role_id",
    "playing_role",
]

def clean(v):
    if pd.isna(v):
        return ""
    return str(v).strip()

def bowling_signal(row):
    for c in ["t20s_bowl_wkts", "t20s_bowl_overs", "t20s_bowl_balls_derived", "t20s_bowl_innings"]:
        v = clean(row.get(c, ""))
        if v and v not in {"0", "0.0"}:
            return True
    return False

def review_reason(row):
    reasons = []

    bio_error = clean(row.get("bio_error", ""))
    bio_quality = clean(row.get("bio_quality", "")).lower()
    bio_needs_retry = clean(row.get("bio_needs_retry", "")).lower() in {"true", "1", "yes", "y"}

    if bio_error or bio_quality == "error" or bio_needs_retry:
        reasons.append("bio_error")
    if bio_quality == "partial":
        reasons.append("bio_partial")
    if not clean(row.get("final_date_of_birth", "")):
        reasons.append("missing_dob")
    if not clean(row.get("standard_batting_code", "")):
        reasons.append("missing_batting_style")
    if not clean(row.get("standard_bowling_code", "")) and bowling_signal(row):
        reasons.append("missing_bowling_style_for_bowler")
    if clean(row.get("playing_role_id", "")).upper() == "UKN" or clean(row.get("playing_role", "")).lower() == "unknown":
        reasons.append("unknown_role")
    if clean(row.get("final_player_status", "")).lower() in {"", "unknown"}:
        reasons.append("missing_or_unknown_status")

    return "; ".join(reasons)

def main():
    if not INPUT_CSV.exists():
        raise FileNotFoundError(f"Input CSV not found: {INPUT_CSV}")

    df = pd.read_csv(INPUT_CSV, dtype=str).fillna("")
    df["_review_reason"] = df.apply(review_reason, axis=1)

    review = df[df["_review_reason"].str.strip() != ""].copy()

    context_cols = [
        "cricinfo_id", "input_name", "final_full_name", "final_display_name", "final_country_name",
        "bio_quality", "bio_error", "_review_reason",
        "final_date_of_birth", "final_date_of_death", "final_player_status", "status_source", "last_played_year",
        "standard_batting_code", "standard_batting_style", "final_batting_style_raw",
        "standard_bowling_code", "standard_bowling_style", "final_bowling_style_raw",
        "playing_role_id", "playing_role", "final_playing_role_raw",
        "t20s_bat_matches", "t20s_bat_runs", "t20s_bat_avg", "t20s_bat_sr",
        "t20s_bowl_matches", "t20s_bowl_overs", "t20s_bowl_wkts", "t20s_bowl_avg", "t20s_bowl_econ",
    ]
    context_cols = [c for c in context_cols if c in review.columns]

    out = review[context_cols].copy()

    for field in EDITABLE_FIELDS:
        out[f"edit_{field}"] = ""

    out["edit_note"] = ""

    out.to_csv(REVIEW_CSV, index=False)

    out[out["_review_reason"].str.contains("bio_error|bio_partial|missing_dob", regex=True, na=False)].to_csv(
        BIO_REVIEW_CSV, index=False
    )

    out[out["_review_reason"].str.contains("missing_batting_style|missing_bowling_style", regex=True, na=False)].to_csv(
        STYLE_REVIEW_CSV, index=False
    )

    report = pd.DataFrame([
        {"item": "input_rows", "value": len(df)},
        {"item": "manual_review_rows", "value": len(out)},
        {"item": "bio_error_rows", "value": int(df["_review_reason"].str.contains("bio_error", na=False).sum())},
        {"item": "bio_partial_rows", "value": int(df["_review_reason"].str.contains("bio_partial", na=False).sum())},
        {"item": "missing_dob_rows", "value": int(df["_review_reason"].str.contains("missing_dob", na=False).sum())},
        {"item": "missing_batting_style_rows", "value": int(df["_review_reason"].str.contains("missing_batting_style", na=False).sum())},
        {"item": "missing_bowling_style_for_bowler_rows", "value": int(df["_review_reason"].str.contains("missing_bowling_style_for_bowler", na=False).sum())},
        {"item": "unknown_role_rows", "value": int(df["_review_reason"].str.contains("unknown_role", na=False).sum())},
        {"item": "missing_or_unknown_status_rows", "value": int(df["_review_reason"].str.contains("missing_or_unknown_status", na=False).sum())},
    ])
    report.to_csv(REPORT_CSV, index=False)

    print(report.to_string(index=False))
    print(f"\nSaved: {REVIEW_CSV}")
    print(f"Saved: {BIO_REVIEW_CSV}")
    print(f"Saved: {STYLE_REVIEW_CSV}")
    print(f"Saved: {REPORT_CSV}")

if __name__ == "__main__":
    main()