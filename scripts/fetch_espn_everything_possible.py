import os
import re
import json
import time
from pathlib import Path

import pandas as pd
import requests


# ============================================================
# INPUTS
# ============================================================

INPUT_INDEX_ENRICHED = Path("outputs/player_index_enriched.csv")
INPUT_INDEX_BASE = Path("outputs/player_index.csv")

OUT_DIR = Path("outputs")
CACHE_DIR = OUT_DIR / "espn_core_cache"
TEAM_CACHE_DIR = OUT_DIR / "espn_team_cache"
HOME_CACHE_DIR = OUT_DIR / "espn_home_cache"

OUT_PROFILES_CSV = OUT_DIR / "espn_everything_profiles.csv"
OUT_PROFILES_JSON = OUT_DIR / "espn_everything_profiles.json"
OUT_TEAMS_CSV = OUT_DIR / "espn_team_reference.csv"
OUT_HOME_PROBE_CSV = OUT_DIR / "espn_home_probe.csv"
OUT_ERRORS_CSV = OUT_DIR / "espn_everything_errors.csv"
OUT_REPORT_CSV = OUT_DIR / "espn_everything_report.csv"

ATHLETE_URL = "http://core.espnuk.org/v2/sports/cricket/athletes/{player_id}"
TEAM_URL = "http://core.espnuk.org/v2/sports/cricket/teams/{team_id}"
HOME_URL = "https://hs-consumer-api.espncricinfo.com/v1/pages/player/home?playerId={player_id}"

HEADERS = {
    "User-Agent": "Mozilla/5.0",
    "Accept": "application/json,text/plain,*/*",
    "Accept-Language": "en-US,en;q=0.9",
}

SLEEP_SECONDS = float(os.getenv("SLEEP_SECONDS", "0.35"))
MAX_PLAYERS = int(os.getenv("MAX_PLAYERS", "0"))  # 0 = all players
FETCH_HOME = os.getenv("FETCH_HOME", "1") == "1"
RESOLVE_TEAMS = os.getenv("RESOLVE_TEAMS", "1") == "1"


# ============================================================
# HELPERS
# ============================================================

def clean_text(x):
    if x is None:
        return ""
    x = str(x)
    x = re.sub(r"\s+", " ", x).strip()
    if x.lower() in {"nan", "none", "null"}:
        return ""
    return x


def normalize_id(x):
    x = clean_text(x)
    x = re.sub(r"\.0$", "", x)
    return x


def safe_get(d, *keys, default=""):
    cur = d
    for key in keys:
        if not isinstance(cur, dict):
            return default
        cur = cur.get(key)
    if cur is None:
        return default
    return cur


def ref_to_id(ref):
    ref = clean_text(ref)
    if not ref:
        return ""
    return ref.rstrip("/").split("/")[-1]


def join_unique(items, sep="|"):
    clean = []
    seen = set()
    for item in items:
        item = clean_text(item)
        if item and item not in seen:
            clean.append(item)
            seen.add(item)
    return sep.join(clean)


def load_player_index():
    if INPUT_INDEX_ENRICHED.exists():
        path = INPUT_INDEX_ENRICHED
    elif INPUT_INDEX_BASE.exists():
        path = INPUT_INDEX_BASE
    else:
        raise FileNotFoundError("Missing outputs/player_index_enriched.csv or outputs/player_index.csv")

    df = pd.read_csv(path, dtype=str).fillna("")
    df.columns = [c.strip() for c in df.columns]

    if "cricinfo_id" not in df.columns:
        raise ValueError("Input file must have cricinfo_id column.")

    df["cricinfo_id"] = df["cricinfo_id"].map(normalize_id)

    df = df[
        (df["cricinfo_id"] != "")
        & (~df["cricinfo_id"].str.startswith("missing_", na=False))
    ].copy()

    df = df.drop_duplicates("cricinfo_id").reset_index(drop=True)

    if MAX_PLAYERS > 0:
        df = df.head(MAX_PLAYERS).copy()

    print("Using:", path)
    print("Players:", len(df))

    return df


def fetch_json(url, cache_path, sleep=True):
    cache_path.parent.mkdir(parents=True, exist_ok=True)

    if cache_path.exists():
        try:
            return json.loads(cache_path.read_text(encoding="utf-8")), "cache"
        except Exception:
            pass

    try:
        r = requests.get(url, headers=HEADERS, timeout=30)
    except Exception as e:
        return {"_error": True, "error_type": "request_exception", "error": str(e), "url": url}, "request_exception"

    if r.status_code != 200:
        data = {
            "_error": True,
            "error_type": f"http_{r.status_code}",
            "status_code": r.status_code,
            "url": url,
            "text_preview": r.text[:800],
        }
        cache_path.write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")
        return data, f"http_{r.status_code}"

    text = r.text.strip()

    try:
        data = json.loads(text)
    except Exception:
        data = {
            "_error": True,
            "error_type": "json_parse_error",
            "url": url,
            "text_preview": text[:1000],
        }
        cache_path.write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")
        return data, "json_parse_error"

    cache_path.write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")

    if sleep:
        time.sleep(SLEEP_SECONDS)

    return data, "fetched"


# ============================================================
# ESPN FETCHERS
# ============================================================

def fetch_athlete(player_id):
    url = ATHLETE_URL.format(player_id=player_id)
    cache_path = CACHE_DIR / f"{player_id}.json"
    return fetch_json(url, cache_path)


def fetch_team(team_id):
    team_id = normalize_id(team_id)
    if not team_id:
        return None, "no_team_id"

    url = TEAM_URL.format(team_id=team_id)
    cache_path = TEAM_CACHE_DIR / f"{team_id}.json"
    return fetch_json(url, cache_path)


def fetch_home(player_id):
    url = HOME_URL.format(player_id=player_id)
    cache_path = HOME_CACHE_DIR / f"{player_id}.json"
    return fetch_json(url, cache_path)


# ============================================================
# PARSERS
# ============================================================

def parse_styles(data):
    batting_style = ""
    batting_style_abbrev = ""
    bowling_style = ""
    bowling_style_abbrev = ""
    all_styles = []

    styles = data.get("styles") or data.get("style") or []

    if isinstance(styles, list):
        for item in styles:
            if not isinstance(item, dict):
                continue

            style_type = clean_text(item.get("type")).lower()
            desc = clean_text(item.get("description"))
            short = clean_text(item.get("shortDescription"))

            if desc:
                all_styles.append(f"{style_type}:{desc}")

            if style_type == "batting":
                batting_style = desc
                batting_style_abbrev = short
            elif style_type == "bowling":
                bowling_style = desc
                bowling_style_abbrev = short

    return {
        "batting_style": batting_style,
        "batting_style_abbrev": batting_style_abbrev,
        "bowling_style": bowling_style,
        "bowling_style_abbrev": bowling_style_abbrev,
        "all_styles": join_unique(all_styles),
    }


def parse_team(team_id, data):
    if not isinstance(data, dict) or data.get("_error"):
        return {
            "team_id": normalize_id(team_id),
            "team_name": "",
            "team_display_name": "",
            "team_short_display_name": "",
            "team_abbreviation": "",
            "team_slug": "",
            "team_color": "",
            "team_logo_url": "",
        }

    logos = data.get("logos") or []
    logo_url = ""
    if isinstance(logos, list) and logos:
        first_logo = logos[0]
        if isinstance(first_logo, dict):
            logo_url = clean_text(first_logo.get("href"))

    return {
        "team_id": clean_text(data.get("id") or team_id),
        "team_name": clean_text(data.get("name")),
        "team_display_name": clean_text(data.get("displayName")),
        "team_short_display_name": clean_text(data.get("shortDisplayName") or data.get("shortName")),
        "team_abbreviation": clean_text(data.get("abbreviation")),
        "team_slug": clean_text(data.get("slug")),
        "team_color": clean_text(data.get("color")),
        "team_logo_url": logo_url,
    }


def resolve_team_name(team_id, team_rows_by_id):
    team_id = normalize_id(team_id)
    if not team_id:
        return ""

    if team_id in team_rows_by_id:
        team = team_rows_by_id[team_id]
        return (
            team.get("team_display_name")
            or team.get("team_name")
            or team.get("team_short_display_name")
            or team.get("team_abbreviation")
            or ""
        )

    data, status = fetch_team(team_id)
    parsed = parse_team(team_id, data)
    parsed["fetch_status"] = status
    team_rows_by_id[team_id] = parsed

    return (
        parsed.get("team_display_name")
        or parsed.get("team_name")
        or parsed.get("team_short_display_name")
        or parsed.get("team_abbreviation")
        or ""
    )


def flatten_interesting_paths(obj, prefix="", rows=None, max_depth=7):
    """
    This does not claim current team.
    It only probes the ESPN player-home JSON for potentially useful fields.
    """
    if rows is None:
        rows = []

    if max_depth < 0:
        return rows

    interesting_words = [
        "team", "teams", "squad", "current", "major",
        "role", "position", "batting", "bowling",
        "style", "country", "profile", "player"
    ]

    if isinstance(obj, dict):
        for k, v in obj.items():
            key = clean_text(k)
            path = f"{prefix}.{key}" if prefix else key

            lower_path = path.lower()

            if any(w in lower_path for w in interesting_words):
                if isinstance(v, (str, int, float, bool)) or v is None:
                    rows.append({
                        "path": path,
                        "value": clean_text(v),
                    })
                elif isinstance(v, dict):
                    simple_name = (
                        v.get("name")
                        or v.get("displayName")
                        or v.get("longName")
                        or v.get("shortName")
                        or v.get("title")
                    )
                    if simple_name:
                        rows.append({
                            "path": path,
                            "value": clean_text(simple_name),
                        })

            flatten_interesting_paths(v, path, rows, max_depth - 1)

    elif isinstance(obj, list):
        for i, item in enumerate(obj[:5]):
            path = f"{prefix}[{i}]"
            flatten_interesting_paths(item, path, rows, max_depth - 1)

    return rows


def parse_home_probe(player_id, home_data):
    if not isinstance(home_data, dict) or home_data.get("_error"):
        return {
            "cricinfo_id": player_id,
            "home_status": "error_or_missing",
            "home_top_keys": "",
            "home_interesting_paths_count": 0,
            "home_interesting_paths_sample": "",
            "explicit_current_team": "",
            "explicit_current_team_confidence": "none",
        }

    top_keys = list(home_data.keys())

    rows = flatten_interesting_paths(home_data)

    sample = []
    explicit_current_team = ""

    for row in rows:
        path = row["path"]
        value = row["value"]
        if not value:
            continue

        sample.append(f"{path}={value}")

        # Conservative only: only trust a key/path that literally says currentTeam.
        compact_path = path.lower().replace("_", "").replace("-", "")
        if "currentteam" in compact_path and not explicit_current_team:
            explicit_current_team = value

    return {
        "cricinfo_id": player_id,
        "home_status": "ok",
        "home_top_keys": join_unique(top_keys),
        "home_interesting_paths_count": len(rows),
        "home_interesting_paths_sample": join_unique(sample[:40]),
        "explicit_current_team": explicit_current_team,
        "explicit_current_team_confidence": "explicit_key" if explicit_current_team else "none",
    }


def parse_athlete(data, source_row, team_rows_by_id, home_probe=None):
    player_id = normalize_id(data.get("id") or source_row.get("cricinfo_id", ""))

    styles = parse_styles(data)

    position = data.get("position") if isinstance(data.get("position"), dict) else {}
    birth_place = data.get("birthPlace") if isinstance(data.get("birthPlace"), dict) else {}

    country_id = normalize_id(data.get("country"))
    country_name = ""
    if RESOLVE_TEAMS and country_id:
        country_name = resolve_team_name(country_id, team_rows_by_id)

    major_team_ids = []
    major_team_names = []

    refs = data.get("majorTeams") or []
    if isinstance(refs, list):
        for ref_obj in refs:
            if not isinstance(ref_obj, dict):
                continue
            tid = ref_to_id(ref_obj.get("$ref"))
            if not tid:
                continue

            major_team_ids.append(tid)

            if RESOLVE_TEAMS:
                name = resolve_team_name(tid, team_rows_by_id)
                if name:
                    major_team_names.append(name)

    headshot_url = safe_get(data, "headshot", "href")
    flag_url = safe_get(data, "flag", "href")

    current_team = ""
    current_team_confidence = "none"
    if home_probe:
        current_team = clean_text(home_probe.get("explicit_current_team"))
        current_team_confidence = clean_text(home_probe.get("explicit_current_team_confidence")) or "none"

    return {
        # Your existing data
        "cricinfo_id": player_id,
        "your_unique_player_id": clean_text(source_row.get("unique_player_id", "")),
        "your_final_player_name": clean_text(source_row.get("final_player_name", "")),
        "your_country_text": clean_text(source_row.get("source_country_text", "")),

        # ESPN names
        "espn_guid": clean_text(data.get("guid")),
        "espn_uid": clean_text(data.get("uid")),
        "espn_type": clean_text(data.get("type")),
        "espn_name": clean_text(data.get("name")),
        "espn_first_name": clean_text(data.get("firstName")),
        "espn_middle_name": clean_text(data.get("middleName")),
        "espn_last_name": clean_text(data.get("lastName")),
        "espn_short_name": clean_text(data.get("shortName")),
        "espn_full_name": clean_text(data.get("fullName")),
        "espn_display_name": clean_text(data.get("displayName")),

        # Bio
        "age": clean_text(data.get("age")),
        "date_of_birth": clean_text(data.get("dateOfBirth")),
        "date_of_birth_str": clean_text(data.get("dateOfBirthStr")),
        "date_of_death": clean_text(data.get("dateOfDeath")),
        "date_of_death_str": clean_text(data.get("dateOfDeathStr")),
        "is_active": clean_text(data.get("isActive")),
        "active": clean_text(data.get("active")),
        "gender": clean_text(data.get("gender")),
        "debut_year": clean_text(data.get("debutYear")),
        "weight": clean_text(data.get("weight")),
        "height": clean_text(data.get("height")),
        "jersey": clean_text(data.get("jersey")),

        # Birth place, often empty but keep it
        "birth_place_city": clean_text(birth_place.get("city")),
        "birth_place_state": clean_text(birth_place.get("state")),
        "birth_place_country": clean_text(birth_place.get("country")),
        "birth_place_raw": json.dumps(birth_place, ensure_ascii=False) if birth_place else "",

        # Styles
        "batting_style": styles["batting_style"],
        "batting_style_abbrev": styles["batting_style_abbrev"],
        "bowling_style": styles["bowling_style"],
        "bowling_style_abbrev": styles["bowling_style_abbrev"],
        "all_styles": styles["all_styles"],

        # Playing role / position
        "playing_role": clean_text(position.get("name")),
        "playing_role_id": clean_text(position.get("id")),
        "playing_role_abbreviation": clean_text(position.get("abbreviation")),

        # Cricket display names
        "batting_name": clean_text(data.get("battingName")),
        "fielding_name": clean_text(data.get("fieldingName")),

        # Country and teams
        "country_id": country_id,
        "country_name": country_name,
        "major_team_ids": join_unique(major_team_ids),
        "major_teams": join_unique(major_team_names),

        # Current team: only if ESPN home endpoint gives an explicit currentTeam key
        "current_team": current_team,
        "current_team_source": "espn_home_explicit_currentTeam" if current_team else "",
        "current_team_confidence": current_team_confidence,

        # Media
        "headshot_url": clean_text(headshot_url),
        "flag_url": clean_text(flag_url),

        # Raw references
        "source_ref": clean_text(data.get("$ref")),
        "relations_count": len(data.get("relations") or []) if isinstance(data.get("relations"), list) else 0,
    }


# ============================================================
# MAIN
# ============================================================

def main():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    players = load_player_index()

    profile_rows = []
    error_rows = []
    home_probe_rows = []
    team_rows_by_id = {}

    total = len(players)

    for idx, source_row in players.iterrows():
        player_id = normalize_id(source_row.get("cricinfo_id", ""))
        player_name = clean_text(source_row.get("final_player_name", ""))

        print(f"[{idx + 1}/{total}] {player_id} {player_name}")

        athlete_data, athlete_status = fetch_athlete(player_id)

        if not isinstance(athlete_data, dict) or athlete_data.get("_error"):
            error_rows.append({
                "cricinfo_id": player_id,
                "final_player_name": player_name,
                "stage": "athlete_core",
                "status": athlete_status,
                "error_type": clean_text(athlete_data.get("error_type") if isinstance(athlete_data, dict) else ""),
                "preview": clean_text(athlete_data.get("text_preview") if isinstance(athlete_data, dict) else ""),
            })
            continue

        home_probe = None

        if FETCH_HOME:
            home_data, home_status = fetch_home(player_id)
            home_probe = parse_home_probe(player_id, home_data)
            home_probe["home_fetch_status"] = home_status
            home_probe_rows.append(home_probe)

        try:
            profile_row = parse_athlete(
                athlete_data,
                source_row,
                team_rows_by_id,
                home_probe=home_probe,
            )
            profile_row["athlete_fetch_status"] = athlete_status
            profile_rows.append(profile_row)
        except Exception as e:
            error_rows.append({
                "cricinfo_id": player_id,
                "final_player_name": player_name,
                "stage": "parse_athlete",
                "status": "exception",
                "error_type": type(e).__name__,
                "preview": str(e),
            })

    profiles_df = pd.DataFrame(profile_rows)
    errors_df = pd.DataFrame(error_rows)
    teams_df = pd.DataFrame(list(team_rows_by_id.values()))
    home_probe_df = pd.DataFrame(home_probe_rows)

    profiles_df.to_csv(OUT_PROFILES_CSV, index=False)
    errors_df.to_csv(OUT_ERRORS_CSV, index=False)
    teams_df.to_csv(OUT_TEAMS_CSV, index=False)
    home_probe_df.to_csv(OUT_HOME_PROBE_CSV, index=False)

    profiles_json = {}
    for _, row in profiles_df.iterrows():
        pid = normalize_id(row.get("cricinfo_id", ""))
        if not pid:
            continue
        profiles_json[pid] = {
            k: (None if pd.isna(v) else v)
            for k, v in row.to_dict().items()
        }

    OUT_PROFILES_JSON.write_text(
        json.dumps(profiles_json, indent=2, ensure_ascii=False),
        encoding="utf-8",
    )

    report = pd.DataFrame([
        {"item": "players_requested", "value": len(players)},
        {"item": "profiles_saved", "value": len(profiles_df)},
        {"item": "errors", "value": len(errors_df)},
        {"item": "teams_resolved", "value": len(teams_df)},
        {"item": "home_probe_rows", "value": len(home_probe_df)},
        {"item": "fetch_home_enabled", "value": FETCH_HOME},
        {"item": "resolve_teams_enabled", "value": RESOLVE_TEAMS},
        {"item": "sleep_seconds", "value": SLEEP_SECONDS},
    ])

    report.to_csv(OUT_REPORT_CSV, index=False)

    print("\nSaved:")
    print(OUT_PROFILES_CSV)
    print(OUT_PROFILES_JSON)
    print(OUT_TEAMS_CSV)
    print(OUT_HOME_PROBE_CSV)
    print(OUT_ERRORS_CSV)
    print(OUT_REPORT_CSV)

    print("\nReport:")
    print(report.to_string(index=False))

    if len(profiles_df):
        print("\nSample:")
        sample_cols = [
            "cricinfo_id",
            "espn_full_name",
            "date_of_birth",
            "batting_style",
            "bowling_style",
            "playing_role",
            "country_name",
            "major_teams",
            "current_team",
            "current_team_confidence",
        ]
        sample_cols = [c for c in sample_cols if c in profiles_df.columns]
        print(profiles_df[sample_cols].head(10).to_string(index=False))


if __name__ == "__main__":
    main()