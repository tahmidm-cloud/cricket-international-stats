import csv
import json
import os
import re
import time
from datetime import datetime
from pathlib import Path

try:
    from dateutil import parser as date_parser
except Exception:
    date_parser = None

from cricdata import CricinfoClient


# ============================================================
# SETTINGS
# ============================================================

INPUT_CSV = Path(os.getenv("INPUT_CSV", "outputs/bio_t20s_only.csv"))
OUTPUT_PREFIX = Path(os.getenv("OUTPUT_PREFIX", "outputs/bio_t20s_more_filled"))

SLEEP_SECONDS = float(os.getenv("SLEEP_SECONDS", "1.0"))
ATTEMPTS = int(os.getenv("ATTEMPTS", "4"))
MAX_FETCH_ROWS = int(os.getenv("MAX_FETCH_ROWS", "0"))
FORCE_FETCH_ALL = os.getenv("FORCE_FETCH_ALL", "0") == "1"
FETCH_UNKNOWN_ROLES = os.getenv("FETCH_UNKNOWN_ROLES", "0") == "1"

CURRENT_YEAR = datetime.now().year

OUT_CSV = OUTPUT_PREFIX.with_suffix(".csv")
OUT_JSON = Path(str(OUTPUT_PREFIX) + "_keyed.json")
OUT_RETRY = Path(str(OUTPUT_PREFIX) + "_still_needs_retry.csv")
OUT_REPORT = Path(str(OUTPUT_PREFIX) + "_report.csv")

for p in [OUT_CSV, OUT_JSON, OUT_RETRY, OUT_REPORT]:
    p.parent.mkdir(parents=True, exist_ok=True)


# ============================================================
# BASIC HELPERS
# ============================================================

def clean_value(x):
    if x is None:
        return ""

    if isinstance(x, (dict, list)):
        return json.dumps(x, ensure_ascii=False)

    x = str(x).strip()
    if x.lower() in {"nan", "none", "null"}:
        return ""
    return x


def blank(x):
    x = clean_value(x)
    return (not x) or x == "-"


def safe_float(x):
    x = clean_value(x).replace(",", "")
    if blank(x):
        return 0.0
    try:
        return float(x)
    except Exception:
        return 0.0


def safe_int(x):
    return int(safe_float(x))


def first_nonblank(*vals):
    for v in vals:
        v = clean_value(v)
        if not blank(v):
            return v
    return ""


def parse_date(value):
    value = clean_value(value)
    if blank(value):
        return ""

    if re.match(r"^\d{4}-\d{2}-\d{2}", value):
        return value[:10]

    if not date_parser:
        return ""

    try:
        return date_parser.parse(value, fuzzy=True).date().isoformat()
    except Exception:
        return ""


def raw_find_deep(obj, keys):
    wanted = {str(k).lower() for k in keys}

    if isinstance(obj, dict):
        for k, v in obj.items():
            if str(k).lower() in wanted and v not in [None, ""]:
                return v
        for v in obj.values():
            found = raw_find_deep(v, keys)
            if found not in [None, ""]:
                return found

    if isinstance(obj, list):
        for item in obj:
            found = raw_find_deep(item, keys)
            if found not in [None, ""]:
                return found

    return ""


def extract_name(value):
    if isinstance(value, dict):
        return first_nonblank(
            value.get("name"),
            value.get("displayName"),
            value.get("fullName"),
            value.get("longName"),
            value.get("label"),
            value.get("description"),
            value.get("abbreviation"),
        )

    if isinstance(value, list):
        names = [extract_name(x) for x in value]
        names = [x for x in names if x]
        return ", ".join(names)

    return clean_value(value)


def extract_id(value):
    if isinstance(value, dict):
        return first_nonblank(
            value.get("id"),
            value.get("abbreviation"),
            value.get("code"),
            value.get("slug"),
        )
    return ""


# ============================================================
# NORMALIZERS
# ============================================================

def normalize_batting_style(raw):
    raw = extract_name(raw)
    low = raw.lower().replace("-", " ")

    if not raw:
        return "", ""

    if "right" in low and ("bat" in low or "hand" in low):
        return "RHB", "Right Hand Bat"

    if "left" in low and ("bat" in low or "hand" in low):
        return "LHB", "Left Hand Bat"

    return "", ""


def normalize_bowling_style(raw):
    raw = extract_name(raw)
    low = raw.lower().replace("-", " ")

    if not raw or low in {"unknown", "right arm bowler", "left arm bowler"}:
        return "", ""

    if "right" in low and "fast medium" in low:
        return "RFM", "Right Arm Fast Medium"
    if "right" in low and "medium fast" in low:
        return "RMF", "Right Arm Medium Fast"
    if "right" in low and "fast" in low:
        return "RF", "Right Arm Fast"
    if "right" in low and ("medium" in low or "slow medium" in low):
        return "RM", "Right Arm Medium"

    if "left" in low and "fast medium" in low:
        return "LFM", "Left Arm Fast Medium"
    if "left" in low and "medium fast" in low:
        return "LMF", "Left Arm Medium Fast"
    if "left" in low and "fast" in low:
        return "LF", "Left Arm Fast"
    if "left" in low and ("medium" in low or "slow medium" in low):
        return "LM", "Left Arm Medium"

    if "offbreak" in low or "off break" in low or ("right" in low and "slow" in low):
        return "OB", "Right Arm Off Break"
    if "legbreak googly" in low or "leg break googly" in low:
        return "LBG", "Leg Break Googly"
    if "legbreak" in low or "leg break" in low:
        return "LB", "Leg Break"
    if "slow left" in low or "left arm orthodox" in low or ("left" in low and "slow" in low):
        return "SLA", "Slow Left Arm Orthodox"
    if "left" in low and "wrist" in low:
        return "LWS", "Left Arm Wrist Spin"

    return "", ""


def normalize_role(raw):
    raw_id = extract_id(raw)
    raw_name = extract_name(raw)
    low = raw_name.lower().replace("-", " ")

    if not raw_name and raw_id:
        raw_name = raw_id
        low = raw_name.lower()

    if not raw_name:
        return "", ""

    if raw_id.upper() == "UKN" or raw_name.lower() == "unknown":
        return "UKN", "Unknown"

    if "opening" in low or low == "opener":
        return "OP", "Opener"
    if "top order" in low:
        return "TBT", "Top-order batter"
    if "middle order" in low:
        return "MBT", "Middle-order batter"

    if "wicketkeeper" in low or "wicket keeper" in low:
        if "batter" in low or "batsman" in low:
            return "WKBT", "Wicketkeeper batter"
        return "WKT", "Wicketkeeper"

    if "batting allrounder" in low or "batting all rounder" in low:
        return "BTAR", "Batting allrounder"
    if "bowling allrounder" in low or "bowling all rounder" in low:
        return "BWAR", "Bowling allrounder"
    if "allrounder" in low or "all rounder" in low:
        return "AR", "Allrounder"

    if "fast bowler" in low:
        return "FB", "Fast bowler"
    if "spinner" in low or "spin bowler" in low:
        return "SPIN", "Spin bowler"
    if "bowler" in low:
        return "BWL", "Bowler"
    if "batter" in low or "batsman" in low:
        return "BAT", "Batter"

    return raw_id, raw_name


ROLE_ID_BY_NAME = {
    "Unknown": "UKN",
    "Batter": "BAT",
    "Bowler": "BWL",
    "Allrounder": "AR",
    "Batting allrounder": "BTAR",
    "Bowling allrounder": "BWAR",
    "Wicketkeeper": "WKT",
    "Wicketkeeper batter": "WKBT",
    "Opener": "OP",
    "Top-order batter": "TBT",
    "Middle-order batter": "MBT",
}


# ============================================================
# BIO FETCHING
# ============================================================

def fetch_bio_with_retries(ci, player_id):
    last_error = ""

    for attempt in range(1, ATTEMPTS + 1):
        try:
            return ci.player_bio(int(player_id)), ""
        except Exception as exc:
            last_error = str(exc)
            wait = SLEEP_SECONDS * attempt * 2
            print(f"Bio retry {attempt}/{ATTEMPTS} failed for {player_id}: {last_error}. Sleeping {wait:.1f}s")
            time.sleep(wait)

    return None, last_error


def extract_bio_fields(bio):
    if not isinstance(bio, (dict, list)):
        return {}

    full_name_raw = raw_find_deep(bio, [
        "fullName", "full_name", "longName", "long_name", "name",
    ])
    display_name_raw = raw_find_deep(bio, [
        "displayName", "display_name", "shortName", "short_name", "popularName",
    ])
    first_name_raw = raw_find_deep(bio, ["firstName", "first_name"])
    last_name_raw = raw_find_deep(bio, ["lastName", "last_name"])
    country_raw = raw_find_deep(bio, [
        "country", "countryName", "country_name", "team", "teamName", "nationality",
    ])

    dob = parse_date(raw_find_deep(bio, [
        "dateOfBirth", "date_of_birth", "displayDOB", "dob", "birthDate", "born",
    ]))
    dod = parse_date(raw_find_deep(bio, [
        "dateOfDeath", "date_of_death", "displayDOD", "dod", "deathDate", "died",
    ]))

    batting_raw = raw_find_deep(bio, [
        "battingStyle", "batting_style", "batStyle", "battingHand", "batting_hand",
    ])
    bowling_raw = raw_find_deep(bio, [
        "bowlingStyle", "bowling_style", "bowlStyle", "bowlingHand", "bowling_hand",
    ])
    role_raw = raw_find_deep(bio, [
        "playingRole", "playing_role", "playingRoles", "playerRole", "role", "roleName", "position",
    ])

    bat_code, bat_style = normalize_batting_style(batting_raw)
    bowl_code, bowl_style = normalize_bowling_style(bowling_raw)
    role_id, role = normalize_role(role_raw)

    return {
        "final_full_name": extract_name(full_name_raw),
        "final_display_name": extract_name(display_name_raw),
        "final_first_name": extract_name(first_name_raw),
        "final_last_name": extract_name(last_name_raw),
        "final_country_name": extract_name(country_raw),
        "final_date_of_birth": dob,
        "final_date_of_death": dod,
        "final_batting_style_raw": extract_name(batting_raw),
        "standard_batting_code": bat_code,
        "standard_batting_style": bat_style,
        "final_bowling_style_raw": extract_name(bowling_raw),
        "standard_bowling_code": bowl_code,
        "standard_bowling_style": bowl_style,
        "final_playing_role_raw": extract_name(role_raw),
        "playing_role_id": role_id,
        "playing_role": role,
    }


# ============================================================
# INFERENCE HELPERS
# ============================================================

def meaningful_bowling(row):
    wkts = safe_float(row.get("t20s_bowl_wkts"))
    overs = safe_float(row.get("t20s_bowl_overs"))
    innings = safe_float(row.get("t20s_bowl_innings"))
    return wkts > 0 or overs >= 12 or innings >= 5


def infer_role_from_t20(row):
    runs = safe_float(row.get("t20s_bat_runs"))
    bat_inns = safe_float(row.get("t20s_bat_innings"))
    bat_avg = safe_float(row.get("t20s_bat_avg"))
    wkts = safe_float(row.get("t20s_bowl_wkts"))
    bowl_inns = safe_float(row.get("t20s_bowl_innings"))
    overs = safe_float(row.get("t20s_bowl_overs"))
    stumpings = safe_float(row.get("t20s_field_stumpings"))

    if stumpings >= 1:
        if runs >= 50 or bat_inns >= 5:
            return "WKBT", "Wicketkeeper batter"
        return "WKT", "Wicketkeeper"

    if wkts >= 10 and (runs >= 250 or (bat_inns >= 20 and bat_avg >= 12)):
        return "AR", "Allrounder"

    if wkts >= 5 or bowl_inns >= 5 or overs >= 20:
        return "BWL", "Bowler"

    if runs >= 50 or bat_inns >= 5:
        return "BAT", "Batter"

    return "UKN", "Unknown"


def overs_to_balls(overs):
    overs = clean_value(overs)
    if blank(overs):
        return ""

    try:
        if "." in overs:
            whole, balls = overs.split(".", 1)
            whole = int(whole)
            balls = int(balls[:1])
            if balls > 5:
                return ""
            return str((whole * 6) + balls)
        return str(int(float(overs)) * 6)
    except Exception:
        return ""


def derive_dismissals(row):
    existing = clean_value(row.get("t20s_field_dismissals"))
    if not blank(existing):
        return existing

    catches = safe_int(row.get("t20s_field_catches"))
    stumpings = safe_int(row.get("t20s_field_stumpings"))

    if blank(row.get("t20s_field_catches")) and blank(row.get("t20s_field_stumpings")):
        return ""

    return str(catches + stumpings)


def should_fetch_bio(row):
    if FORCE_FETCH_ALL:
        return True

    if not blank(row.get("bio_error")):
        return True

    if blank(row.get("final_country_name")):
        return True

    if blank(row.get("final_date_of_birth")):
        return True

    if blank(row.get("standard_batting_code")):
        return True

    if blank(row.get("standard_bowling_code")) and meaningful_bowling(row):
        return True

    if FETCH_UNKNOWN_ROLES and clean_value(row.get("playing_role")) in {"", "Unknown"}:
        return True

    return False


# ============================================================
# ROW UPDATE
# ============================================================

def apply_bio_fields(row, bio_fields, source):
    filled = 0

    for col, new_val in bio_fields.items():
        new_val = clean_value(new_val)
        if blank(new_val):
            continue

        old_val = clean_value(row.get(col))
        if blank(old_val) or old_val == "Unknown":
            row[col] = new_val
            filled += 1

    if source and filled:
        row["bio_fill_source"] = source

    return filled


def apply_inference(row):
    # Derive bowling balls.
    if blank(row.get("t20s_bowl_balls_derived")):
        row["t20s_bowl_balls_derived"] = first_nonblank(
            row.get("t20s_bowl_balls"),
            overs_to_balls(row.get("t20s_bowl_overs")),
        )

    # Derive fielding dismissals.
    if blank(row.get("t20s_field_dismissals_derived")):
        row["t20s_field_dismissals_derived"] = derive_dismissals(row)

    # Fill role only when unknown/blank using T20 evidence.
    if clean_value(row.get("playing_role")) in {"", "Unknown"}:
        role_id, role = infer_role_from_t20(row)
        if role != "Unknown":
            row["playing_role_id"] = role_id
            row["playing_role"] = role
            row["playing_role_fill_source"] = "t20_stats_inferred"
        else:
            row["playing_role_id"] = first_nonblank(row.get("playing_role_id"), "UKN")
            row["playing_role"] = "Unknown"
            row["playing_role_fill_source"] = first_nonblank(row.get("playing_role_fill_source"), "unknown_not_enough_data")
    else:
        row["playing_role_fill_source"] = first_nonblank(row.get("playing_role_fill_source"), "source")

    # Fill true non-bowlers so your game does not treat them as missing data.
    if blank(row.get("standard_bowling_code")):
        if not meaningful_bowling(row):
            row["standard_bowling_code"] = "NOBOWL"
            row["standard_bowling_style"] = "Non-bowler"
            row["bowling_style_fill_source"] = "inferred_non_bowler"
        else:
            row["bowling_style_fill_source"] = "still_missing_but_player_bowled"
    else:
        row["bowling_style_fill_source"] = first_nonblank(row.get("bowling_style_fill_source"), "source")

    # Make a game-friendly status without overwriting original final_player_status.
    status = clean_value(row.get("final_player_status"))
    last_year = safe_int(row.get("last_played_year"))
    if status == "unknown" and last_year and last_year <= CURRENT_YEAR - 4:
        row["game_player_status"] = "retired"
        row["game_status_source"] = "unknown_old_last_played_game_rule"
    else:
        row["game_player_status"] = status
        row["game_status_source"] = clean_value(row.get("status_source"))

    # Recompute bio quality.
    key_missing = []
    for col in ["final_country_name", "final_date_of_birth", "standard_batting_code"]:
        if blank(row.get(col)):
            key_missing.append(col)

    if not blank(row.get("bio_error")):
        row["bio_quality"] = "error"
    elif key_missing:
        row["bio_quality"] = "partial"
    else:
        row["bio_quality"] = "good"

    row["bio_needs_retry"] = "true" if row["bio_quality"] in {"error", "partial"} else "false"

    row["batting_style_needs_manual_review"] = "true" if blank(row.get("standard_batting_code")) else "false"
    row["bowling_style_needs_manual_review"] = "true" if row.get("bowling_style_fill_source") == "still_missing_but_player_bowled" else "false"


# ============================================================
# IO + REPORT
# ============================================================

def read_csv(path):
    with path.open("r", newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        return reader.fieldnames or [], rows


def write_csv(path, rows, fieldnames):
    final_fields = list(fieldnames)
    for row in rows:
        for key in row.keys():
            if key not in final_fields:
                final_fields.append(key)

    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=final_fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)

    return final_fields


def count_filled(rows, col):
    return sum(1 for r in rows if not blank(r.get(col)))


def write_report(rows, fetched, bio_success, bio_failed):
    report = []
    def add(item, value):
        report.append({"item": item, "value": value})

    add("rows", len(rows))
    add("bio_fetch_attempted", fetched)
    add("bio_fetch_success", bio_success)
    add("bio_fetch_failed", bio_failed)

    for col in [
        "final_country_name",
        "final_date_of_birth",
        "final_date_of_death",
        "standard_batting_code",
        "standard_bowling_code",
        "playing_role_id",
        "playing_role",
        "t20s_bowl_4w",
        "t20s_bowl_5w",
        "t20s_bowl_balls_derived",
        "t20s_field_dismissals_derived",
    ]:
        add(f"filled_{col}", count_filled(rows, col))

    for val in ["good", "partial", "error"]:
        add(f"bio_quality_{val}", sum(1 for r in rows if clean_value(r.get("bio_quality")) == val))

    for val in ["active", "retired", "passed_away", "unknown"]:
        add(f"game_status_{val}", sum(1 for r in rows if clean_value(r.get("game_player_status")) == val))

    add("batting_style_needs_manual_review", sum(1 for r in rows if clean_value(r.get("batting_style_needs_manual_review")) == "true"))
    add("bowling_style_needs_manual_review", sum(1 for r in rows if clean_value(r.get("bowling_style_needs_manual_review")) == "true"))

    write_csv(OUT_REPORT, report, ["item", "value"])


def row_to_keyed_record(row):
    return {
        "profile": {
            "cricinfo_id": row.get("cricinfo_id", ""),
            "name": row.get("final_full_name", ""),
            "display_name": row.get("final_display_name", ""),
            "country": row.get("final_country_name", ""),
            "date_of_birth": row.get("final_date_of_birth", ""),
            "date_of_death": row.get("final_date_of_death", ""),
            "player_status": row.get("final_player_status", ""),
            "game_player_status": row.get("game_player_status", ""),
            "batting_code": row.get("standard_batting_code", ""),
            "batting_style": row.get("standard_batting_style", ""),
            "bowling_code": row.get("standard_bowling_code", ""),
            "bowling_style": row.get("standard_bowling_style", ""),
            "playing_role_id": row.get("playing_role_id", ""),
            "playing_role": row.get("playing_role", ""),
            "bio_quality": row.get("bio_quality", ""),
        },
        "t20s": {
            "batting": {
                "matches": row.get("t20s_bat_matches", ""),
                "innings": row.get("t20s_bat_innings", ""),
                "runs": row.get("t20s_bat_runs", ""),
                "average": row.get("t20s_bat_avg", ""),
                "strike_rate": row.get("t20s_bat_sr", ""),
                "fours": row.get("t20s_bat_4s", ""),
                "sixes": row.get("t20s_bat_6s", ""),
            },
            "bowling": {
                "matches": row.get("t20s_bowl_matches", ""),
                "overs": row.get("t20s_bowl_overs", ""),
                "balls": row.get("t20s_bowl_balls_derived", ""),
                "wickets": row.get("t20s_bowl_wkts", ""),
                "average": row.get("t20s_bowl_avg", ""),
                "economy": row.get("t20s_bowl_econ", ""),
                "strike_rate": row.get("t20s_bowl_sr", ""),
                "four_wicket_hauls": row.get("t20s_bowl_4w", ""),
                "five_wicket_hauls": row.get("t20s_bowl_5w", ""),
            },
            "fielding": {
                "catches": row.get("t20s_field_catches", ""),
                "stumpings": row.get("t20s_field_stumpings", ""),
                "dismissals": row.get("t20s_field_dismissals_derived", ""),
            },
        },
    }


# ============================================================
# MAIN
# ============================================================

def main():
    if not INPUT_CSV.exists():
        raise FileNotFoundError(f"Missing input CSV: {INPUT_CSV}")

    fieldnames, rows = read_csv(INPUT_CSV)
    print(f"Loaded {len(rows)} rows from {INPUT_CSV}")

    to_fetch = [r for r in rows if should_fetch_bio(r)]
    if MAX_FETCH_ROWS > 0:
        to_fetch = to_fetch[:MAX_FETCH_ROWS]

    fetch_ids = {clean_value(r.get("cricinfo_id")) for r in to_fetch}
    print(f"Bio rows selected for retry/fill: {len(fetch_ids)}")

    ci = CricinfoClient()
    fetched = 0
    bio_success = 0
    bio_failed = 0

    for row in rows:
        player_id = clean_value(row.get("cricinfo_id"))

        if player_id in fetch_ids:
            fetched += 1
            bio, err = fetch_bio_with_retries(ci, player_id)

            if bio:
                fields = extract_bio_fields(bio)
                filled = apply_bio_fields(row, fields, "cricinfo_bio_retry")
                row["bio_error"] = ""
                row["bio_retry_result"] = f"success_filled_{filled}"
                bio_success += 1
            else:
                row["bio_error"] = err
                row["bio_retry_result"] = "failed"
                bio_failed += 1

            time.sleep(SLEEP_SECONDS)

        apply_inference(row)

    final_fields = write_csv(OUT_CSV, rows, fieldnames)

    keyed = {
        clean_value(r.get("cricinfo_id")): row_to_keyed_record(r)
        for r in rows
        if not blank(r.get("cricinfo_id"))
    }
    OUT_JSON.write_text(json.dumps(keyed, indent=2, ensure_ascii=False), encoding="utf-8")

    retry_rows = [
        r for r in rows
        if clean_value(r.get("bio_needs_retry")) == "true"
        or clean_value(r.get("batting_style_needs_manual_review")) == "true"
        or clean_value(r.get("bowling_style_needs_manual_review")) == "true"
    ]
    write_csv(OUT_RETRY, retry_rows, final_fields)

    write_report(rows, fetched, bio_success, bio_failed)

    print("Saved:")
    print(OUT_CSV)
    print(OUT_JSON)
    print(OUT_RETRY)
    print(OUT_REPORT)


if __name__ == "__main__":
    main()