import json
import os
import re
import time
from datetime import datetime
from pathlib import Path

import pandas as pd
from dateutil import parser as date_parser
from tqdm import tqdm

from cricdata import CricinfoClient


PLAYER_INDEX_CANDIDATES = [
    Path("outputs/player_index_espn_slim.csv"),
    Path("outputs/player_index_enriched.csv"),
    Path("outputs/player_index.csv"),
]

EXISTING_INTERNATIONAL_STATS = Path("outputs/all_international_stats_enriched.json")

OUT_DIR = Path("outputs")
SHARD_DIR = OUT_DIR / "shards"
SHARD_DIR.mkdir(parents=True, exist_ok=True)

SHARD_INDEX = int(os.getenv("SHARD_INDEX", "0"))
SHARD_TOTAL = int(os.getenv("SHARD_TOTAL", "1"))

MAX_PLAYERS = int(os.getenv("MAX_PLAYERS", "0"))
TEST_ID = os.getenv("TEST_ID", "").strip()
SLEEP_SECONDS = float(os.getenv("SLEEP_SECONDS", "0.60"))
FORCE_REFRESH = os.getenv("FORCE_REFRESH", "0") == "1"

CURRENT_YEAR = datetime.now().year
T20_FORMAT = "t20"

CACHE_DIR = OUT_DIR / "bio_t20s_only_cache" / f"shard_{SHARD_INDEX}_of_{SHARD_TOTAL}"
CACHE_DIR.mkdir(parents=True, exist_ok=True)

OUT_CSV = SHARD_DIR / f"bio_t20s_only_shard_{SHARD_INDEX}_of_{SHARD_TOTAL}.csv"
OUT_JSON = SHARD_DIR / f"bio_t20s_only_shard_{SHARD_INDEX}_of_{SHARD_TOTAL}_keyed.json"
OUT_ERRORS = SHARD_DIR / f"bio_t20s_only_shard_{SHARD_INDEX}_of_{SHARD_TOTAL}_errors.csv"
OUT_REPORT = SHARD_DIR / f"bio_t20s_only_shard_{SHARD_INDEX}_of_{SHARD_TOTAL}_report.csv"


def clean_value(x):
    if x is None:
        return ""

    try:
        if pd.isna(x):
            return ""
    except Exception:
        pass

    x = str(x).strip()

    if x.lower() in {"nan", "none", "null", "unknown", "not available"}:
        return ""

    return x


def clean_id(x):
    return clean_value(x).replace(".0", "").strip()


def parse_date(value):
    value = clean_value(value)

    if not value:
        return ""

    if "T" in value and re.match(r"^\d{4}-\d{2}-\d{2}", value):
        return value[:10]

    if re.match(r"^\d{4}-\d{2}-\d{2}$", value):
        return value

    try:
        return date_parser.parse(value, fuzzy=True).date().isoformat()
    except Exception:
        return ""


def find_deep(obj, keys):
    wanted = {k.lower() for k in keys}

    if isinstance(obj, dict):
        for k, v in obj.items():
            if str(k).lower() in wanted:
                val = clean_value(v)
                if val:
                    return val

        for v in obj.values():
            found = find_deep(v, keys)
            if found:
                return found

    elif isinstance(obj, list):
        for item in obj:
            found = find_deep(item, keys)
            if found:
                return found

    return ""


def normalize_batting_style(raw):
    raw = clean_value(raw)
    low = raw.lower().replace("-", " ")

    if "right" in low and "bat" in low:
        return "RHB", "Right Hand Bat"

    if "left" in low and "bat" in low:
        return "LHB", "Left Hand Bat"

    return "", ""


def normalize_bowling_style(raw):
    raw = clean_value(raw)
    low = raw.lower().replace("-", " ")

    if not raw or low in {"right arm bowler", "left arm bowler", "unknown"}:
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
    raw = clean_value(raw)
    low = raw.lower()

    if not raw:
        return "", ""

    if "opening" in low or low == "opener":
        return "OP", "Opener"
    if "top-order" in low or "top order" in low:
        return "TBT", "Top-order batter"
    if "middle-order" in low or "middle order" in low:
        return "MBT", "Middle-order batter"
    if "wicketkeeper" in low or "wicket-keeper" in low:
        if "batter" in low or "batsman" in low:
            return "WKBT", "Wicketkeeper batter"
        return "WKT", "Wicketkeeper"
    if "batting allrounder" in low or "batting all-rounder" in low:
        return "BTAR", "Batting allrounder"
    if "bowling allrounder" in low or "bowling all-rounder" in low:
        return "BWAR", "Bowling allrounder"
    if "allrounder" in low or "all-rounder" in low:
        return "AR", "Allrounder"
    if "fast bowler" in low:
        return "FB", "Fast bowler"
    if "spinner" in low or "spin bowler" in low:
        return "SPIN", "Spin bowler"
    if "bowler" in low:
        return "BWL", "Bowler"
    if "batter" in low or "batsman" in low:
        return "BAT", "Batter"

    return "", raw


def cache_path(player_id):
    return CACHE_DIR / f"{player_id}.json"


def load_cached(player_id):
    path = cache_path(player_id)

    if FORCE_REFRESH or not path.exists():
        return None

    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return None


def save_cached(player_id, record):
    cache_path(player_id).write_text(
        json.dumps(record, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )


def load_player_index():
    index_path = None

    for path in PLAYER_INDEX_CANDIDATES:
        if path.exists():
            index_path = path
            break

    if not index_path:
        raise FileNotFoundError(
            "Missing player index. Expected one of: "
            + ", ".join(str(p) for p in PLAYER_INDEX_CANDIDATES)
        )

    df = pd.read_csv(index_path, dtype=str).fillna("")

    if "cricinfo_id" not in df.columns:
        raise ValueError(f"{index_path} missing cricinfo_id column")

    df["cricinfo_id"] = df["cricinfo_id"].map(clean_id)

    name_cols = [
        "final_player_name",
        "espn_full_name",
        "espn_display_name",
        "your_final_player_name",
        "name",
    ]

    def choose_name(row):
        for col in name_cols:
            if col in row.index:
                val = clean_value(row.get(col))
                if val:
                    return val
        return f"player-{row.get('cricinfo_id', '')}"

    df["input_name"] = df.apply(choose_name, axis=1)

    df = df[df["cricinfo_id"] != ""].copy()
    df = df.drop_duplicates("cricinfo_id", keep="first").copy()

    if TEST_ID:
        df = df[df["cricinfo_id"] == TEST_ID].copy()

    if MAX_PLAYERS > 0:
        df = df.head(MAX_PLAYERS).copy()

    # shard split
    if not TEST_ID and SHARD_TOTAL > 1:
        df = df.iloc[SHARD_INDEX::SHARD_TOTAL].copy()

    return df, index_path


def load_existing_stats():
    if not EXISTING_INTERNATIONAL_STATS.exists():
        return {}

    try:
        return json.loads(EXISTING_INTERNATIONAL_STATS.read_text(encoding="utf-8"))
    except Exception:
        return {}


def get_last_year_from_existing_stats(stats_player):
    years = []

    if not isinstance(stats_player, dict):
        return ""

    for type_key in ["batting", "bowling", "fielding"]:
        bucket = stats_player.get(type_key) or {}

        if not isinstance(bucket, dict):
            continue

        for _, row in bucket.items():
            if not isinstance(row, dict):
                continue

            for key in ["End", "end", "last_played_year", "LastPlayedYear"]:
                val = clean_value(row.get(key))
                if re.fullmatch(r"\d{4}", val):
                    years.append(int(val))

            span = clean_value(row.get("Span"))
            if span:
                found = re.findall(r"\b(18\d{2}|19\d{2}|20\d{2})\b", span)
                years.extend(int(y) for y in found)

    return max(years) if years else ""


def get_last_year_from_t20s(t20s):
    years = []

    def scan(obj):
        if isinstance(obj, dict):
            for k, v in obj.items():
                key = str(k).lower()

                if key in {"end", "lastyear", "last_year", "lastplayed", "last_played"}:
                    val = clean_value(v)
                    if re.fullmatch(r"\d{4}", val):
                        years.append(int(val))

                if key == "span":
                    found = re.findall(r"\b(18\d{2}|19\d{2}|20\d{2})\b", clean_value(v))
                    years.extend(int(y) for y in found)

                scan(v)

        elif isinstance(obj, list):
            for item in obj:
                scan(item)

    scan(t20s)
    return max(years) if years else ""


def infer_status(date_of_death, last_year):
    if date_of_death:
        return "passed_away", "death_date"

    if last_year:
        last_year = int(last_year)

        if last_year >= CURRENT_YEAR - 3:
            return "active", "stats_recent_match"

        if last_year <= CURRENT_YEAR - 5:
            return "retired", "stats_last_played_old"

        return "unknown", "stats_last_played_uncertain"

    return "unknown", "no_status_signal"


def extract_bio_fields(bio):
    full_name = (
        find_deep(bio, ["fullName", "full_name", "name"])
        or find_deep(bio, ["displayName", "display_name"])
    )

    display_name = find_deep(bio, ["displayName", "display_name", "shortName", "short_name"])
    first_name = find_deep(bio, ["firstName", "first_name"])
    last_name = find_deep(bio, ["lastName", "last_name"])
    country = find_deep(bio, ["country", "countryName", "country_name", "team", "teamName"])

    dob = parse_date(find_deep(bio, ["dateOfBirth", "date_of_birth", "displayDOB", "dob", "birthDate", "born"]))
    dod = parse_date(find_deep(bio, ["dateOfDeath", "date_of_death", "displayDOD", "dod", "deathDate", "died"]))

    batting_raw = find_deep(bio, ["battingStyle", "batting_style", "batting", "batStyle"])
    bowling_raw = find_deep(bio, ["bowlingStyle", "bowling_style", "bowling", "bowlStyle"])
    role_raw = find_deep(bio, ["playingRole", "playing_role", "role", "position", "playerType"])

    bat_code, bat_style = normalize_batting_style(batting_raw)
    bowl_code, bowl_style = normalize_bowling_style(bowling_raw)
    role_id, role = normalize_role(role_raw)

    return {
        "full_name": clean_value(full_name),
        "display_name": clean_value(display_name),
        "first_name": clean_value(first_name),
        "last_name": clean_value(last_name),
        "country": clean_value(country),
        "date_of_birth": dob,
        "date_of_death": dod,
        "batting_style_raw": clean_value(batting_raw),
        "bowling_style_raw": clean_value(bowling_raw),
        "playing_role_raw": clean_value(role_raw),
        "standard_batting_code": bat_code,
        "standard_batting_style": bat_style,
        "standard_bowling_code": bowl_code,
        "standard_bowling_style": bowl_style,
        "playing_role_id": role_id,
        "playing_role": role,
    }


def flatten_summary(data):
    if not isinstance(data, dict):
        return {}

    if isinstance(data.get("summary"), dict):
        return data["summary"]

    keep_keys = [
        "Span", "Start", "End",
        "Mat", "Matches",
        "Inns", "Innings",
        "NO", "NotOuts",
        "Runs", "HS", "HighScore",
        "Ave", "Average",
        "BF", "BallsFaced",
        "SR", "StrikeRate",
        "100", "100s", "Hundreds",
        "50", "50s", "Fifties",
        "0", "Ducks",
        "4s", "Fours",
        "6s", "Sixes",
        "Balls", "Overs",
        "Mdns", "Maidens",
        "Wkts", "Wickets",
        "BBI", "BestBowlingInnings",
        "BBM", "BestBowlingMatch",
        "Econ", "Economy",
        "4w", "FourWickets",
        "5w", "FiveWickets",
        "10w", "TenWickets",
        "Ct", "Caught",
        "St", "Stumped",
        "Dismissals",
    ]

    out = {}

    for key in keep_keys:
        val = clean_value(data.get(key))
        if val:
            out[key] = val

    return out


def fetch_t20s_stats(ci, player_id):
    out = {
        "format_key": T20_FORMAT,
        "label": "T20s",
        "batting": {},
        "bowling": {},
        "fielding": {},
        "errors": {},
    }

    for stat_type in ["batting", "bowling", "fielding"]:
        try:
            data = ci.player_career_stats(
                int(player_id),
                fmt=T20_FORMAT,
                stat_type=stat_type,
            )
            out[stat_type] = flatten_summary(data)

        except Exception as exc:
            out["errors"][stat_type] = str(exc)

        time.sleep(SLEEP_SECONDS)

    return out


def get_stat(t20s, stat_type, *keys):
    obj = t20s.get(stat_type, {})

    for key in keys:
        val = clean_value(obj.get(key))
        if val:
            return val

    return ""


def flatten_record(record):
    p = record["profile"]
    t = record["t20s"]

    row = dict(p)

    row["t20s_bat_matches"] = get_stat(t, "batting", "Matches", "Mat")
    row["t20s_bat_innings"] = get_stat(t, "batting", "Innings", "Inns")
    row["t20s_bat_not_outs"] = get_stat(t, "batting", "NotOuts", "NO")
    row["t20s_bat_runs"] = get_stat(t, "batting", "Runs")
    row["t20s_bat_high_score"] = get_stat(t, "batting", "HighScore", "HS")
    row["t20s_bat_avg"] = get_stat(t, "batting", "Average", "Ave")
    row["t20s_bat_balls_faced"] = get_stat(t, "batting", "BallsFaced", "BF")
    row["t20s_bat_sr"] = get_stat(t, "batting", "StrikeRate", "SR")
    row["t20s_bat_100s"] = get_stat(t, "batting", "Hundreds", "100s", "100")
    row["t20s_bat_50s"] = get_stat(t, "batting", "Fifties", "50s", "50")
    row["t20s_bat_ducks"] = get_stat(t, "batting", "Ducks", "0")
    row["t20s_bat_4s"] = get_stat(t, "batting", "Fours", "4s")
    row["t20s_bat_6s"] = get_stat(t, "batting", "Sixes", "6s")

    row["t20s_bowl_matches"] = get_stat(t, "bowling", "Matches", "Mat")
    row["t20s_bowl_innings"] = get_stat(t, "bowling", "Innings", "Inns")
    row["t20s_bowl_balls"] = get_stat(t, "bowling", "Balls")
    row["t20s_bowl_overs"] = get_stat(t, "bowling", "Overs")
    row["t20s_bowl_maidens"] = get_stat(t, "bowling", "Maidens", "Mdns")
    row["t20s_bowl_runs"] = get_stat(t, "bowling", "RunsConceded", "Runs")
    row["t20s_bowl_wkts"] = get_stat(t, "bowling", "Wickets", "Wkts")
    row["t20s_bowl_bbi"] = get_stat(t, "bowling", "BestBowlingInnings", "BBI")
    row["t20s_bowl_bbm"] = get_stat(t, "bowling", "BestBowlingMatch", "BBM")
    row["t20s_bowl_avg"] = get_stat(t, "bowling", "Average", "Ave")
    row["t20s_bowl_econ"] = get_stat(t, "bowling", "Economy", "Econ")
    row["t20s_bowl_sr"] = get_stat(t, "bowling", "StrikeRate", "SR")
    row["t20s_bowl_4w"] = get_stat(t, "bowling", "FourWickets", "4w")
    row["t20s_bowl_5w"] = get_stat(t, "bowling", "FiveWickets", "5w")
    row["t20s_bowl_10w"] = get_stat(t, "bowling", "TenWickets", "10w")

    row["t20s_field_catches"] = get_stat(t, "fielding", "Caught", "Ct")
    row["t20s_field_stumpings"] = get_stat(t, "fielding", "Stumped", "St")
    row["t20s_field_dismissals"] = get_stat(t, "fielding", "Dismissals")

    row["t20s_batting_error"] = clean_value(t.get("errors", {}).get("batting"))
    row["t20s_bowling_error"] = clean_value(t.get("errors", {}).get("bowling"))
    row["t20s_fielding_error"] = clean_value(t.get("errors", {}).get("fielding"))

    return row


def fetch_one(ci, player_id, input_name, existing_stats):
    cached = load_cached(player_id)

    if cached is not None:
        return cached

    bio_error = ""

    try:
        bio = ci.player_bio(int(player_id))
        bio_fields = extract_bio_fields(bio)

    except Exception as exc:
        bio_error = str(exc)
        bio_fields = {}

    time.sleep(SLEEP_SECONDS)

    t20s = fetch_t20s_stats(ci, player_id)

    last_year_t20s = get_last_year_from_t20s(t20s)
    last_year_international = get_last_year_from_existing_stats(existing_stats.get(player_id, {}))
    last_year = last_year_t20s or last_year_international

    status, status_source = infer_status(
        bio_fields.get("date_of_death", ""),
        last_year,
    )

    profile = {
        "cricinfo_id": player_id,
        "input_name": input_name,

        "final_full_name": bio_fields.get("full_name") or input_name,
        "final_display_name": bio_fields.get("display_name") or input_name,
        "final_first_name": bio_fields.get("first_name", ""),
        "final_last_name": bio_fields.get("last_name", ""),
        "final_country_name": bio_fields.get("country", ""),

        "final_date_of_birth": bio_fields.get("date_of_birth", ""),
        "final_date_of_death": bio_fields.get("date_of_death", ""),

        "final_player_status": status,
        "status_source": status_source,
        "last_played_year": last_year,
        "last_played_year_t20s": last_year_t20s,
        "last_played_year_international": last_year_international,

        "final_batting_style_raw": bio_fields.get("batting_style_raw", ""),
        "standard_batting_code": bio_fields.get("standard_batting_code", ""),
        "standard_batting_style": bio_fields.get("standard_batting_style", ""),

        "final_bowling_style_raw": bio_fields.get("bowling_style_raw", ""),
        "standard_bowling_code": bio_fields.get("standard_bowling_code", ""),
        "standard_bowling_style": bio_fields.get("standard_bowling_style", ""),

        "final_playing_role_raw": bio_fields.get("playing_role_raw", ""),
        "playing_role_id": bio_fields.get("playing_role_id", ""),
        "playing_role": bio_fields.get("playing_role", ""),

        "bio_error": bio_error,
    }

    record = {
        "profile": profile,
        "t20s": t20s,
    }

    save_cached(player_id, record)
    return record


def main():
    players, index_path = load_player_index()
    existing_stats = load_existing_stats()

    print("Using player index:", index_path)
    print("Shard:", SHARD_INDEX, "of", SHARD_TOTAL)
    print("Players in this shard:", len(players))
    print("Sleep:", SLEEP_SECONDS)
    print("Force refresh:", FORCE_REFRESH)

    ci = CricinfoClient()

    keyed = {}
    rows = []
    errors = []

    for _, player in tqdm(players.iterrows(), total=len(players)):
        player_id = player["cricinfo_id"]
        input_name = player["input_name"]

        try:
            record = fetch_one(ci, player_id, input_name, existing_stats)
            keyed[player_id] = record
            rows.append(flatten_record(record))

        except Exception as exc:
            errors.append({
                "cricinfo_id": player_id,
                "input_name": input_name,
                "error": str(exc),
            })

        time.sleep(SLEEP_SECONDS)

    out_df = pd.DataFrame(rows)
    err_df = pd.DataFrame(errors)

    out_df.to_csv(OUT_CSV, index=False)

    OUT_JSON.write_text(
        json.dumps(keyed, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    err_df.to_csv(OUT_ERRORS, index=False)

    report = pd.DataFrame([
        {"item": "shard_index", "value": SHARD_INDEX},
        {"item": "shard_total", "value": SHARD_TOTAL},
        {"item": "players_in_shard", "value": len(players)},
        {"item": "rows_saved", "value": len(out_df)},
        {"item": "errors", "value": len(err_df)},
        {"item": "with_t20s_batting", "value": int((out_df.get("t20s_bat_matches", "") != "").sum()) if len(out_df) else 0},
        {"item": "with_t20s_bowling", "value": int((out_df.get("t20s_bowl_matches", "") != "").sum()) if len(out_df) else 0},
        {"item": "status_active", "value": int((out_df.get("final_player_status", "") == "active").sum()) if len(out_df) else 0},
        {"item": "status_retired", "value": int((out_df.get("final_player_status", "") == "retired").sum()) if len(out_df) else 0},
        {"item": "status_passed_away", "value": int((out_df.get("final_player_status", "") == "passed_away").sum()) if len(out_df) else 0},
        {"item": "status_unknown", "value": int((out_df.get("final_player_status", "") == "unknown").sum()) if len(out_df) else 0},
    ])

    report.to_csv(OUT_REPORT, index=False)

    print("Saved:")
    print(OUT_CSV)
    print(OUT_JSON)
    print(OUT_ERRORS)
    print(OUT_REPORT)
    print(report.to_string(index=False))


if __name__ == "__main__":
    main()