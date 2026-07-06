import json
import os
import pandas as pd
from pathlib import Path

ORIGINAL_CSV = Path(os.getenv("ORIGINAL_CSV", "outputs/bio_t20s_more_filled.csv"))
EDITED_REVIEW_CSV = Path(os.getenv("EDITED_REVIEW_CSV", "outputs/bio_t20s_manual_review_needed_edited.csv"))
OUTPUT_CSV = Path(os.getenv("OUTPUT_CSV", "outputs/bio_t20s_final.csv"))
OUTPUT_JSON = Path(os.getenv("OUTPUT_JSON", "outputs/bio_t20s_final_keyed.json"))
CHANGE_LOG_CSV = Path(os.getenv("CHANGE_LOG_CSV", "outputs/bio_t20s_manual_review_change_log.csv"))

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

VALID_STATUS = {"active", "retired", "passed_away", "unknown"}
VALID_BAT_CODES = {"RHB", "LHB"}
VALID_BOWL_CODES = {"NOBOWL", "RF", "RFM", "RMF", "RM", "LF", "LFM", "LMF", "LM", "OB", "LBG", "LB", "SLA", "LWS"}

BATTING_STYLE_FROM_CODE = {
    "RHB": "Right Hand Bat",
    "LHB": "Left Hand Bat",
}

BOWLING_STYLE_FROM_CODE = {
    "NOBOWL": "Does Not Bowl",
    "RF": "Right Arm Fast",
    "RFM": "Right Arm Fast Medium",
    "RMF": "Right Arm Medium Fast",
    "RM": "Right Arm Medium",
    "LF": "Left Arm Fast",
    "LFM": "Left Arm Fast Medium",
    "LMF": "Left Arm Medium Fast",
    "LM": "Left Arm Medium",
    "OB": "Right Arm Off Break",
    "LBG": "Leg Break Googly",
    "LB": "Leg Break",
    "SLA": "Slow Left Arm Orthodox",
    "LWS": "Left Arm Wrist Spin",
}

ROLE_FROM_ID = {
    "UKN": "Unknown",
    "OP": "Opener",
    "TBT": "Top-order batter",
    "MBT": "Middle-order batter",
    "WKBT": "Wicketkeeper batter",
    "WKT": "Wicketkeeper",
    "BTAR": "Batting allrounder",
    "BWAR": "Bowling allrounder",
    "AR": "Allrounder",
    "FB": "Fast bowler",
    "SPIN": "Spin bowler",
    "BWL": "Bowler",
    "BAT": "Batter",
}

def clean(v):
    if pd.isna(v):
        return ""
    return str(v).strip()

def normalize_code(v):
    return clean(v).upper()

def apply_pair_defaults(row):
    bat_code = normalize_code(row.get("standard_batting_code", ""))
    if bat_code in BATTING_STYLE_FROM_CODE and not clean(row.get("standard_batting_style", "")):
        row["standard_batting_style"] = BATTING_STYLE_FROM_CODE[bat_code]

    bowl_code = normalize_code(row.get("standard_bowling_code", ""))
    if bowl_code in BOWLING_STYLE_FROM_CODE and not clean(row.get("standard_bowling_style", "")):
        row["standard_bowling_style"] = BOWLING_STYLE_FROM_CODE[bowl_code]

    role_id = normalize_code(row.get("playing_role_id", ""))
    if role_id in ROLE_FROM_ID and not clean(row.get("playing_role", "")):
        row["playing_role"] = ROLE_FROM_ID[role_id]

    return row

def validate_edit(field, value):
    value = clean(value)

    if not value:
        return value

    if field == "final_player_status":
        v = value.lower()
        if v not in VALID_STATUS:
            raise ValueError(f"Invalid final_player_status: {value}")
        return v

    if field == "standard_batting_code":
        v = value.upper()
        if v not in VALID_BAT_CODES:
            raise ValueError(f"Invalid batting code: {value}")
        return v

    if field == "standard_bowling_code":
        v = value.upper()
        if v not in VALID_BOWL_CODES:
            raise ValueError(f"Invalid bowling code: {value}")
        return v

    if field == "playing_role_id":
        return value.upper()

    return value

def main():
    if not ORIGINAL_CSV.exists():
        raise FileNotFoundError(f"Original CSV not found: {ORIGINAL_CSV}")

    if not EDITED_REVIEW_CSV.exists():
        raise FileNotFoundError(
            f"Edited review file not found: {EDITED_REVIEW_CSV}\n"
            "Edit outputs/bio_t20s_manual_review_needed.csv, save it as "
            "outputs/bio_t20s_manual_review_needed_edited.csv, then rerun."
        )

    original = pd.read_csv(ORIGINAL_CSV, dtype=str).fillna("")
    edits = pd.read_csv(EDITED_REVIEW_CSV, dtype=str).fillna("")

    if "cricinfo_id" not in original.columns or "cricinfo_id" not in edits.columns:
        raise ValueError("Both files must contain cricinfo_id")

    original["cricinfo_id"] = original["cricinfo_id"].astype(str).str.strip()
    edits["cricinfo_id"] = edits["cricinfo_id"].astype(str).str.strip()

    original = original.drop_duplicates("cricinfo_id", keep="first").copy()
    edits = edits.drop_duplicates("cricinfo_id", keep="last").copy()

    original = original.set_index("cricinfo_id", drop=False)
    edits = edits.set_index("cricinfo_id", drop=False)

    changes = []

    for player_id, edit_row in edits.iterrows():
        if player_id not in original.index:
            continue

        for field in EDITABLE_FIELDS:
            edit_col = f"edit_{field}"

            if edit_col not in edits.columns:
                continue

            new_value = validate_edit(field, edit_row.get(edit_col, ""))
            if new_value == "":
                continue

            old_value = clean(original.at[player_id, field]) if field in original.columns else ""

            if field not in original.columns:
                original[field] = ""

            if old_value != new_value:
                original.at[player_id, field] = new_value
                changes.append({
                    "cricinfo_id": player_id,
                    "input_name": clean(original.at[player_id, "input_name"]) if "input_name" in original.columns else "",
                    "field": field,
                    "old_value": old_value,
                    "new_value": new_value,
                    "edit_note": clean(edit_row.get("edit_note", "")),
                })

        # Fill matching style/name after code edits.
        row_dict = original.loc[player_id].to_dict()
        row_dict = apply_pair_defaults(row_dict)
        for k, v in row_dict.items():
            if k in original.columns:
                original.at[player_id, k] = v

    final = original.reset_index(drop=True)

    # Clear manual flags where fixed.
    if "bio_needs_retry" in final.columns:
        final.loc[final["bio_error"].astype(str).str.strip() == "", "bio_needs_retry"] = "False"

    OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    final.to_csv(OUTPUT_CSV, index=False)

    keyed = {}
    for _, row in final.iterrows():
        pid = clean(row.get("cricinfo_id", ""))
        if not pid:
            continue
        keyed[pid] = row.to_dict()

    OUTPUT_JSON.write_text(json.dumps(keyed, indent=2, ensure_ascii=False), encoding="utf-8")
    pd.DataFrame(changes).to_csv(CHANGE_LOG_CSV, index=False)

    print(f"Rows saved: {len(final)}")
    print(f"Changes applied: {len(changes)}")
    print(f"Saved: {OUTPUT_CSV}")
    print(f"Saved: {OUTPUT_JSON}")
    print(f"Saved: {CHANGE_LOG_CSV}")

if __name__ == "__main__":
    main()