import json
import os
import re
from io import StringIO
from pathlib import Path

import pandas as pd
import requests
from bs4 import BeautifulSoup


PLAYER_ID = os.getenv("PLAYER_ID", "8166").strip()
PLAYER_SLUG = os.getenv("PLAYER_SLUG", "shane-warne").strip()

OUT_DIR = Path("outputs/cricinfo_stats_page_inspect")
OUT_DIR.mkdir(parents=True, exist_ok=True)

URLS = [
    f"https://www.cricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}/bowling-batting-stats",
    f"https://www.espncricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}/bowling-batting-stats",
    f"https://www.cricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}",
    f"https://www.espncricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}",
    f"https://www.espncricinfo.com/ci/content/player/{PLAYER_ID}.html",
]

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/126.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,application/json;q=0.8,*/*;q=0.7",
    "Accept-Language": "en-US,en;q=0.9",
    "Referer": "https://www.espncricinfo.com/",
}


KEYWORDS = [
    "Tests",
    "ODIs",
    "T20Is",
    "First-class",
    "FC",
    "List A",
    "T20s",
    "Batting & Fielding",
    "Bowling",
    "Mat",
    "Inns",
    "Runs",
    "Wkts",
    "Ave",
    "SR",
]


def clean_text(x):
    return re.sub(r"\s+", " ", str(x)).strip()


def save_text(path, text):
    Path(path).write_text(text, encoding="utf-8", errors="ignore")


def get_next_data(html):
    soup = BeautifulSoup(html, "lxml")
    script = soup.find("script", id="__NEXT_DATA__")

    if not script:
        return {}

    raw = script.string or script.get_text() or ""

    if not raw.strip():
        return {}

    try:
        return json.loads(raw)
    except Exception:
        return {}


def walk_json(obj, path="root", hits=None):
    if hits is None:
        hits = []

    if isinstance(obj, dict):
        preview_parts = []

        for k, v in obj.items():
            if not isinstance(v, (dict, list)):
                preview_parts.append(f"{k}: {v}")

        preview = clean_text(" ".join(preview_parts))
        low = preview.lower()

        if any(k.lower() in low for k in KEYWORDS):
            hits.append({
                "path": path,
                "type": "dict",
                "preview": preview[:500],
                "keys": list(obj.keys()),
            })

        for k, v in obj.items():
            if isinstance(v, (dict, list)):
                walk_json(v, f"{path}.{k}", hits)

    elif isinstance(obj, list):
        preview_parts = []

        for item in obj:
            if not isinstance(item, (dict, list)):
                preview_parts.append(str(item))

        preview = clean_text(" ".join(preview_parts))
        low = preview.lower()

        if any(k.lower() in low for k in KEYWORDS):
            hits.append({
                "path": path,
                "type": f"list[{len(obj)}]",
                "preview": preview[:500],
                "keys": [],
            })

        for i, item in enumerate(obj):
            if isinstance(item, (dict, list)):
                walk_json(item, f"{path}[{i}]", hits)

    return hits


def extract_tables(html):
    tables_out = []

    try:
        tables = pd.read_html(StringIO(html))
    except Exception as exc:
        return [], str(exc)

    for i, df in enumerate(tables):
        df = df.fillna("")

        if isinstance(df.columns, pd.MultiIndex):
            df.columns = [
                " ".join([str(x) for x in col if str(x) != "nan"]).strip()
                for col in df.columns
            ]

        df.columns = [str(c).strip() for c in df.columns]

        preview = df.head(12).to_dict(orient="records")

        tables_out.append({
            "table_index": i,
            "shape": list(df.shape),
            "columns": list(df.columns),
            "preview": preview,
        })

    return tables_out, ""


def main():
    report = []

    for idx, url in enumerate(URLS):
        label = f"url_{idx + 1}"
        print(f"\nTrying {label}: {url}")

        try:
            r = requests.get(url, headers=HEADERS, timeout=30, allow_redirects=True)
            status = r.status_code
            html = r.text or ""
        except Exception as exc:
            report.append({
                "label": label,
                "url": url,
                "status": "ERROR",
                "error": str(exc),
            })
            continue

        html_path = OUT_DIR / f"{label}_{PLAYER_ID}.html"
        save_text(html_path, html)

        text = BeautifulSoup(html, "lxml").get_text("\n")
        text_clean = clean_text(text)

        text_path = OUT_DIR / f"{label}_{PLAYER_ID}_text.txt"
        save_text(text_path, text_clean)

        next_data = get_next_data(html)
        next_path = OUT_DIR / f"{label}_{PLAYER_ID}_next_data.json"
        save_text(next_path, json.dumps(next_data, indent=2, ensure_ascii=False, default=str))

        hits = walk_json(next_data) if next_data else []
        hits_path = OUT_DIR / f"{label}_{PLAYER_ID}_json_hits.json"
        save_text(hits_path, json.dumps(hits, indent=2, ensure_ascii=False, default=str))

        tables, table_error = extract_tables(html)
        tables_path = OUT_DIR / f"{label}_{PLAYER_ID}_tables.json"
        save_text(tables_path, json.dumps(tables, indent=2, ensure_ascii=False, default=str))

        keyword_hits = {}

        for keyword in KEYWORDS:
            keyword_hits[keyword] = keyword.lower() in text_clean.lower()

        report.append({
            "label": label,
            "url": url,
            "final_url": r.url,
            "status": status,
            "html_chars": len(html),
            "text_chars": len(text_clean),
            "has_next_data": bool(next_data),
            "json_hit_count": len(hits),
            "table_count": len(tables),
            "table_error": table_error,
            "keyword_hits": keyword_hits,
            "saved_html": str(html_path),
            "saved_text": str(text_path),
            "saved_next_data": str(next_path),
            "saved_json_hits": str(hits_path),
            "saved_tables": str(tables_path),
        })

        print("Status:", status)
        print("Final URL:", r.url)
        print("HTML chars:", len(html))
        print("Has __NEXT_DATA__:", bool(next_data))
        print("JSON hits:", len(hits))
        print("Tables:", len(tables))
        print("Keyword hits:", {k: v for k, v in keyword_hits.items() if v})

    report_path = OUT_DIR / f"{PLAYER_ID}_inspect_report.json"
    save_text(report_path, json.dumps(report, indent=2, ensure_ascii=False, default=str))

    summary_path = OUT_DIR / f"{PLAYER_ID}_summary.txt"
    lines = []

    for item in report:
        lines.append("=" * 90)
        lines.append(f"{item.get('label')}: {item.get('url')}")
        lines.append(f"status: {item.get('status')}")
        lines.append(f"final_url: {item.get('final_url', '')}")
        lines.append(f"html_chars: {item.get('html_chars', '')}")
        lines.append(f"has_next_data: {item.get('has_next_data', '')}")
        lines.append(f"json_hit_count: {item.get('json_hit_count', '')}")
        lines.append(f"table_count: {item.get('table_count', '')}")
        lines.append(f"table_error: {item.get('table_error', '')}")
        lines.append(f"keyword_hits: {item.get('keyword_hits', '')}")
        lines.append("")

    save_text(summary_path, "\n".join(lines))

    print("\nSaved report:")
    print(report_path)
    print(summary_path)


if __name__ == "__main__":
    main()