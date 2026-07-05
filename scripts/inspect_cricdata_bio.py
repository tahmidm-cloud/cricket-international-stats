import json
import os
from pathlib import Path

from cricdata import CricinfoClient


PLAYER_ID = os.getenv("TEST_ID", "8166").strip()
PLAYER_NAME = os.getenv("PLAYER_NAME", "").strip()

OUT_DIR = Path("outputs/bio_inspect")
OUT_DIR.mkdir(parents=True, exist_ok=True)


KEYWORDS = [
    "first-class",
    "first class",
    "firstclass",
    "fc",
    "list a",
    "lista",
    "list-a",
    "t20s",
    "twenty20",
    "twenty20s",
    "career averages",
    "career averages",
    "batting & fielding",
    "batting",
    "bowling",
    "fielding",
    "format",
    "mat",
    "matches",
    "inns",
    "innings",
    "runs",
    "wickets",
    "wkts",
    "span",
    "ave",
    "average",
    "strike rate",
    "sr",
]


def short(value, limit=260):
    text = str(value).replace("\n", " ").replace("\r", " ")
    text = " ".join(text.split())

    if len(text) > limit:
        return text[:limit] + "..."

    return text


def safe_json(value):
    try:
        json.dumps(value)
        return value
    except Exception:
        return str(value)


def object_text(obj):
    if isinstance(obj, dict):
        parts = []

        for k, v in obj.items():
            if isinstance(v, (dict, list)):
                continue
            parts.append(f"{k}: {v}")

        return " ".join(parts)

    if isinstance(obj, list):
        parts = []

        for item in obj:
            if isinstance(item, (dict, list)):
                continue
            parts.append(str(item))

        return " ".join(parts)

    return str(obj)


def walk_hits(obj, path="root", hits=None):
    if hits is None:
        hits = []

    text = object_text(obj)
    low = text.lower()

    if any(k in low for k in KEYWORDS):
        hits.append({
            "path": path,
            "type": type(obj).__name__,
            "preview": short(text),
            "raw": safe_json(obj),
        })

    if isinstance(obj, dict):
        for k, v in obj.items():
            if isinstance(v, (dict, list)):
                walk_hits(v, f"{path}.{k}", hits)

    elif isinstance(obj, list):
        for i, item in enumerate(obj):
            if isinstance(item, (dict, list)):
                walk_hits(item, f"{path}[{i}]", hits)

    return hits


def walk_all_paths(obj, path="root", rows=None):
    if rows is None:
        rows = []

    if isinstance(obj, dict):
        rows.append({
            "path": path,
            "type": "dict",
            "keys": list(obj.keys())[:40],
            "preview": short(object_text(obj)),
        })

        for k, v in obj.items():
            if isinstance(v, (dict, list)):
                walk_all_paths(v, f"{path}.{k}", rows)

    elif isinstance(obj, list):
        rows.append({
            "path": path,
            "type": f"list[{len(obj)}]",
            "keys": [],
            "preview": short(object_text(obj)),
        })

        for i, item in enumerate(obj):
            if isinstance(item, (dict, list)):
                walk_all_paths(item, f"{path}[{i}]", rows)

    return rows


def main():
    print(f"Fetching cricdata bio for ID: {PLAYER_ID}")
    print(f"Player name note: {PLAYER_NAME or '(none)'}")

    ci = CricinfoClient()
    bio = ci.player_bio(int(PLAYER_ID))

    raw_path = OUT_DIR / f"{PLAYER_ID}_raw_bio.json"
    hits_path = OUT_DIR / f"{PLAYER_ID}_keyword_hits.json"
    paths_path = OUT_DIR / f"{PLAYER_ID}_all_paths.json"
    summary_path = OUT_DIR / f"{PLAYER_ID}_summary.txt"

    raw_path.write_text(
        json.dumps(bio, indent=2, ensure_ascii=False, default=str),
        encoding="utf-8"
    )

    hits = walk_hits(bio)
    all_paths = walk_all_paths(bio)

    hits_path.write_text(
        json.dumps(hits, indent=2, ensure_ascii=False, default=str),
        encoding="utf-8"
    )

    paths_path.write_text(
        json.dumps(all_paths, indent=2, ensure_ascii=False, default=str),
        encoding="utf-8"
    )

    summary_lines = [
        f"Player ID: {PLAYER_ID}",
        f"Player name note: {PLAYER_NAME}",
        f"Total keyword hits: {len(hits)}",
        f"Total object/list paths: {len(all_paths)}",
        "",
        "TOP KEYWORD HITS:",
        "",
    ]

    for hit in hits[:120]:
        summary_lines.append(f"PATH: {hit['path']}")
        summary_lines.append(f"TYPE: {hit['type']}")
        summary_lines.append(f"PREVIEW: {hit['preview']}")
        summary_lines.append("-" * 80)

    summary_path.write_text("\n".join(summary_lines), encoding="utf-8")

    print("\nSaved files:")
    print(raw_path)
    print(hits_path)
    print(paths_path)
    print(summary_path)

    print("\nTop hits:")
    for hit in hits[:40]:
        print("\nPATH:", hit["path"])
        print("TYPE:", hit["type"])
        print("PREVIEW:", hit["preview"])


if __name__ == "__main__":
    main()