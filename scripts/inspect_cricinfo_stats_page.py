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
    f"https://www.espncricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}/bowling-batting-stats",
    f"https://www.cricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}/bowling-batting-stats",
    f"https://www.espncricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}",
    f"https://www.cricinfo.com/cricketers/{PLAYER_SLUG}-{PLAYER_ID}",
    f"https://www.espncricinfo.com/ci/content/player/{PLAYER_ID}.html",
]

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/126.0 Safari/537.36"
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


def clean_text(value):
    return re.sub(r"\s+", " ", str(value)).strip()


def save_text(path, text):
    Path(path).write_text(text, encoding="utf-8", errors="ignore")


def extract_next_data(html):
    soup = BeautifulSoup(html, "lxml")
    tag = soup.find("script", id="__NEXT_DATA__")

    if not tag:
        return {}

    raw = tag.string or tag.get_text() or ""

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
        preview = clean_text(
            " ".join(
                f"{k}: {v}"
                for k, v in obj.items()
                if not isinstance(v, (dict, list))
            )
        )

        low = preview.lower()

        if any(keyword.lower() in low for keyword in KEYWORDS):
            hits.append({
                "path": path,
                "type": "dict",
                "keys": list(obj.keys()),
                "preview": preview[:700],
            })

        for key, value in obj.items():
            if isinstance(value, (dict, list)):
                walk_json(value, f"{path}.{key}", hits)

    elif isinstance(obj, list):
        preview = clean_text(
            " ".join(
                str(item)
                for item in obj
                if not isinstance(item, (dict, list))
            )
        )

        low = preview.lower()

        if any(keyword.lower() in low for keyword in KEYWORDS):
            hits.append({
                "path": path,
                "type": f"list[{len(obj)}]",
                "keys": [],
                "preview": preview[:700],
            })

        for index, item in enumerate(obj):
            if isinstance(item, (dict, list)):
                walk_json(item, f"{path}[{index}]", hits)

    return hits


def extract_tables(html):
    try:
        tables = pd.read_html(StringIO(html))
    except Exception as exc:
        return [], str(exc)

    output = []

    for index, df in enumerate(tables):
        df = df.fillna("")

        if isinstance(df.columns, pd.MultiIndex):
            df.columns = [
                " ".join([str(x) for x in col if str(x) != "nan"]).strip()
                for col in df.columns
            ]

        df.columns = [str(col).strip() for col in df.columns]

        output.append({
            "table_index": index,
            "shape": list(df.shape),
            "columns": list(df.columns),
            "preview": df.head(20).to_dict(orient="records"),
        })

    return output, ""


def main():
    report = []

    for index, url in enumerate(URLS, start=1):
        label = f"url_{index}"

        print("=" * 80)
        print("Trying:", url)

        try:
            response = requests.get(
                url,
                headers=HEADERS,
                timeout=30,
                allow_redirects=True,
            )

            status = response.status_code
            html = response.text or ""

        except Exception as exc:
            report.append({
                "label": label,
                "url": url,
                "status": "ERROR",
                "error": str(exc),
            })
            continue

        html_path = OUT_DIR / f"{label}_{PLAYER_ID}.html"
        text_path = OUT_DIR / f"{label}_{PLAYER_ID}_text.txt"
        next_data_path = OUT_DIR / f"{label}_{PLAYER_ID}_next_data.json"
        hits_path = OUT_DIR / f"{label}_{PLAYER_ID}_json_hits.json"
        tables_path = OUT_DIR / f"{label}_{PLAYER_ID}_tables.json"

        save_text(html_path, html)

        soup = BeautifulSoup(html, "lxml")
        text = clean_text(soup.get_text("\n"))
        save_text(text_path, text)

        next_data = extract_next_data(html)
        save_text(
            next_data_path,
            json.dumps(next_data, indent=2, ensure_ascii=False, default=str),
        )

        hits = walk_json(next_data) if next_data else []
        save_text(
            hits_path,
            json.dumps(hits, indent=2, ensure_ascii=False, default=str),
        )

        tables, table_error = extract_tables(html)
        save_text(
            tables_path,
            json.dumps(tables, indent=2, ensure_ascii=False, default=str),
        )

        keyword_hits = {
            keyword: keyword.lower() in text.lower()
            for keyword in KEYWORDS
        }

        item = {
            "label": label,
            "url": url,
            "final_url": response.url,
            "status": status,
            "html_chars": len(html),
            "text_chars": len(text),
            "has_next_data": bool(next_data),
            "json_hit_count": len(hits),
            "table_count": len(tables),
            "table_error": table_error,
            "keyword_hits": keyword_hits,
            "saved_html": str(html_path),
            "saved_text": str(text_path),
            "saved_next_data": str(next_data_path),
            "saved_json_hits": str(hits_path),
            "saved_tables": str(tables_path),
        }

        report.append(item)

        print("Status:", status)
        print("Final URL:", response.url)
        print("HTML chars:", len(html))
        print("Text chars:", len(text))
        print("Has NEXT DATA:", bool(next_data))
        print("JSON hits:", len(hits))
        print("Tables:", len(tables))
        print("Positive keywords:", [k for k, v in keyword_hits.items() if v])

    report_path = OUT_DIR / f"{PLAYER_ID}_inspect_report.json"
    summary_path = OUT_DIR / f"{PLAYER_ID}_summary.txt"

    save_text(
        report_path,
        json.dumps(report, indent=2, ensure_ascii=False, default=str),
    )

    lines = []

    for item in report:
        lines.append("=" * 90)
        lines.append(f"label: {item.get('label')}")
        lines.append(f"url: {item.get('url')}")
        lines.append(f"final_url: {item.get('final_url', '')}")
        lines.append(f"status: {item.get('status')}")
        lines.append(f"html_chars: {item.get('html_chars')}")
        lines.append(f"text_chars: {item.get('text_chars')}")
        lines.append(f"has_next_data: {item.get('has_next_data')}")
        lines.append(f"json_hit_count: {item.get('json_hit_count')}")
        lines.append(f"table_count: {item.get('table_count')}")
        lines.append(f"table_error: {item.get('table_error')}")
        lines.append(f"keyword_hits: {item.get('keyword_hits')}")
        lines.append("")

    save_text(summary_path, "\n".join(lines))

    print("\nSaved report:")
    print(report_path)
    print("\nSaved summary:")
    print(summary_path)


if __name__ == "__main__":
    main()