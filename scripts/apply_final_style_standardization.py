import json
from pathlib import Path

import pandas as pd


INPUT = Path("outputs/espn_everything_profiles.csv")

OUT_CSV = Path("outputs/espn_profiles_styles_final.csv")
OUT_JSON = Path("outputs/espn_profiles_styles_final.json")
OUT_REPORT = Path("outputs/espn_styles_final_value_report.csv")


def clean(x):
    if pd.isna(x):
        return ""

    x = str(x).strip()

    if x.lower() in {"nan", "none", "null", "unknown"}:
        return ""

    return x


def standardize_batting(raw):
    raw = clean(raw)

    if raw == "Right-hand bat":
        return {
            "standard_batting_code": "RHB",
            "standard_batting_style": "Right Hand Bat",
            "standard_batting_hand": "Right",
        }

    if raw == "Left-hand bat":
        return {
            "standard_batting_code": "LHB",
            "standard_batting_style": "Left Hand Bat",
            "standard_batting_hand": "Left",
        }

    return {
        "standard_batting_code": "",
        "standard_batting_style": "",
        "standard_batting_hand": "",
    }


def standardize_bowling(raw):
    raw = clean(raw)

    # Blank / no bowling / unknown
    if raw in {
        "",
        "Right-arm bowler",
        "Left-arm bowler",
        "unknown arm slow underarm",
        "(unknown arm) slow (underarm)",
    }:
        return {
            "standard_bowling_code": "",
            "standard_bowling_style": "",
            "bowling_arm": "",
            "bowling_kind": "",
            "pace_class": "",
            "spin_type": "",
            "release_modifier": "",
            "has_bowling_style": False,
            "style_review_flag": "",
        }

    # Pace / seam mappings
    pace_map = {
        "Right-arm fast": ("RF", "Right Arm Fast", "Right", "pace", "fast"),
        "Right-arm fast (roundarm)": ("RF", "Right Arm Fast", "Right", "pace", "fast"),

        "Right-arm fast-medium": ("RFM", "Right Arm Fast Medium", "Right", "pace", "fast-medium"),
        "Right-arm medium-fast": ("RMF", "Right Arm Medium Fast", "Right", "pace", "medium-fast"),

        "Right-arm medium": ("RM", "Right Arm Medium", "Right", "pace", "medium"),
        "Right-arm slow-medium": ("RM", "Right Arm Medium", "Right", "pace", "medium"),

        "Left-arm fast": ("LF", "Left Arm Fast", "Left", "pace", "fast"),
        "Left-arm fast roundarm": ("LF", "Left Arm Fast", "Left", "pace", "fast"),
        "Left-arm fast (roundarm)": ("LF", "Left Arm Fast", "Left", "pace", "fast"),

        "Left-arm fast-medium": ("LFM", "Left Arm Fast Medium", "Left", "pace", "fast-medium"),
        "Left-arm medium-fast": ("LMF", "Left Arm Medium Fast", "Left", "pace", "medium-fast"),

        "Left-arm medium": ("LM", "Left Arm Medium", "Left", "pace", "medium"),
        "Left-arm slow-medium": ("LM", "Left Arm Medium", "Left", "pace", "medium"),
    }

    if raw in pace_map:
        code, style, arm, kind, pace_class = pace_map[raw]

        release = ""
        if "roundarm" in raw.lower():
            release = "roundarm"

        return {
            "standard_bowling_code": code,
            "standard_bowling_style": style,
            "bowling_arm": arm,
            "bowling_kind": kind,
            "pace_class": pace_class,
            "spin_type": "",
            "release_modifier": release,
            "has_bowling_style": True,
            "style_review_flag": "",
        }

    # Spin mappings
    spin_map = {
        "Right-arm offbreak": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),
        "Right-arm offbreak underarm": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),
        "Right-arm offbreak (underarm)": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),

        "Right-arm slow": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),
        "Right-arm slow (underarm)": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),
        "Right-arm slow roundarm": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),
        "Right-arm slow (roundarm)": ("OB", "Right Arm Off Break", "Right", "spin", "offbreak"),

        "Legbreak": ("LB", "Leg Break", "Right", "spin", "legbreak"),
        "Legbreak googly": ("LBG", "Leg Break Googly", "Right", "spin", "legbreak-googly"),

        "Slow left-arm orthodox": ("SLA", "Slow Left Arm Orthodox", "Left", "spin", "orthodox"),
        "Left-arm slow": ("SLA", "Slow Left Arm Orthodox", "Left", "spin", "orthodox"),

        "Left-arm wrist-spin": ("LWS", "Left Arm Wrist Spin", "Left", "spin", "wrist-spin"),
    }

    if raw in spin_map:
        code, style, arm, kind, spin_type = spin_map[raw]

        release = ""
        if "underarm" in raw.lower():
            release = "underarm"
        elif "roundarm" in raw.lower():
            release = "roundarm"

        return {
            "standard_bowling_code": code,
            "standard_bowling_style": style,
            "bowling_arm": arm,
            "bowling_kind": kind,
            "pace_class": "",
            "spin_type": spin_type,
            "release_modifier": release,
            "has_bowling_style": True,
            "style_review_flag": "",
        }

    # Anything unexpected becomes blank and gets flagged
    return {
        "standard_bowling_code": "",
        "standard_bowling_style": "",
        "bowling_arm": "",
        "bowling_kind": "",
        "pace_class": "",
        "spin_type": "",
        "release_modifier": "",
        "has_bowling_style": False,
        "style_review_flag": f"unmapped_raw_bowling_style: {raw}",
    }


def main():
    if not INPUT.exists():
        raise FileNotFoundError(f"Missing {INPUT}")

    df = pd.read_csv(INPUT, dtype=str).fillna("")

    if "batting_style" not in df.columns:
        raise ValueError("Input missing batting_style column")

    if "bowling_style" not in df.columns:
        raise ValueError("Input missing bowling_style column")

    batting_rows = df["batting_style"].apply(standardize_batting).apply(pd.Series)
    bowling_rows = df["bowling_style"].apply(standardize_bowling).apply(pd.Series)

    # Remove old standardization columns if they already exist
    drop_cols = [
        "standard_batting_code",
        "standard_batting_style",
        "standard_batting_hand",
        "standard_bowling_code",
        "standard_bowling_style",
        "bowling_arm",
        "bowling_kind",
        "pace_class",
        "spin_type",
        "release_modifier",
        "has_bowling_style",
        "style_review_flag",
    ]

    df = df.drop(columns=[c for c in drop_cols if c in df.columns], errors="ignore")

    final = pd.concat([df, batting_rows, bowling_rows], axis=1)

    final.to_csv(OUT_CSV, index=False)

    out_json = {}

    for _, row in final.iterrows():
        pid = clean(row.get("cricinfo_id", ""))

        if not pid:
            continue

        out_json[pid] = {
            k: (None if pd.isna(v) else v)
            for k, v in row.to_dict().items()
        }

    OUT_JSON.write_text(
        json.dumps(out_json, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    # Reports
    raw_batting = (
        final["batting_style"]
        .replace("", "(blank)")
        .value_counts()
        .reset_index()
    )
    raw_batting.columns = ["raw_value", "count"]
    raw_batting["field"] = "raw_batting_style"

    raw_bowling = (
        final["bowling_style"]
        .replace("", "(blank)")
        .value_counts()
        .reset_index()
    )
    raw_bowling.columns = ["raw_value", "count"]
    raw_bowling["field"] = "raw_bowling_style"

    standard_batting = (
        final["standard_batting_style"]
        .replace("", "(blank/unknown batting)")
        .value_counts()
        .reset_index()
    )
    standard_batting.columns = ["raw_value", "count"]
    standard_batting["field"] = "standard_batting_style"

    standard_bowling = (
        final["standard_bowling_style"]
        .replace("", "(blank/no bowling)")
        .value_counts()
        .reset_index()
    )
    standard_bowling.columns = ["raw_value", "count"]
    standard_bowling["field"] = "standard_bowling_style"

    report = pd.concat(
        [raw_batting, raw_bowling, standard_batting, standard_bowling],
        ignore_index=True,
    )
    report = report[["field", "raw_value", "count"]]
    report.to_csv(OUT_REPORT, index=False)

    print("Input rows:", len(df))
    print("Output rows:", len(final))
    print("Saved:", OUT_CSV)
    print("Saved:", OUT_JSON)
    print("Saved:", OUT_REPORT)

    print("\nStandard batting styles:")
    print(
        final["standard_batting_style"]
        .replace("", "(blank/unknown batting)")
        .value_counts()
        .to_string()
    )

    print("\nStandard bowling styles:")
    print(
        final["standard_bowling_style"]
        .replace("", "(blank/no bowling)")
        .value_counts()
        .to_string()
    )

    print("\nBowling kind counts:")
    print(
        final["bowling_kind"]
        .replace("", "(blank/no bowling)")
        .value_counts()
        .to_string()
    )

    flagged = final[final["style_review_flag"].astype(str).str.strip() != ""]

    print("\nReview flags:")
    print("flagged rows:", len(flagged))

    if len(flagged):
        print(
            flagged[
                [
                    "cricinfo_id",
                    "espn_full_name",
                    "bowling_style",
                    "style_review_flag",
                ]
            ].head(100).to_string(index=False)
        )


if __name__ == "__main__":
    main()