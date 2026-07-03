import re
import json
import requests
from bs4 import BeautifulSoup

URL = "https://www.cricbuzz.com/profiles/1413/virat-kohli"

HEADERS = {
    "User-Agent": "Mozilla/5.0",
    "Accept-Language": "en-US,en;q=0.9",
}


def clean(text):
    if text is None:
        return None
    text = re.sub(r"\s+", " ", text).strip()
    return text or None


def get_next_value_by_label(soup, label):
    label_node = soup.find(string=re.compile(rf"^\s*{re.escape(label)}\s*$", re.I))
    if not label_node:
        return None

    parent = label_node.parent
    if not parent:
        return None

    # Cricbuzz profile layout often has label div followed by value div.
    container = parent.parent
    if not container:
        return None

    texts = [clean(x.get_text(" ", strip=True)) for x in container.find_all(["div", "span"])]
    texts = [x for x in texts if x]

    for i, t in enumerate(texts):
        if t.lower() == label.lower() and i + 1 < len(texts):
            return texts[i + 1]

    return None


def parse_profile(html, url):
    soup = BeautifulSoup(html, "html.parser")

    title = clean(soup.find("title").get_text(" ", strip=True)) if soup.find("title") else None

    name = None
    country = None

    # The page has visible player name text; title is reliable fallback.
    if title:
        name = title.split(" Profile")[0].strip()

    # Try to find country around the header.
    body_text = clean(soup.get_text(" ", strip=True)) or ""

    profile = {
        "source": "cricbuzz",
        "profile_url": url,
        "title": title,
        "cricbuzz_name": name,
        "country": None,
        "born": get_next_value_by_label(soup, "Born"),
        "birth_place": get_next_value_by_label(soup, "Birth Place"),
        "height": get_next_value_by_label(soup, "Height"),
        "role": get_next_value_by_label(soup, "Role"),
        "batting_style": get_next_value_by_label(soup, "Batting Style"),
        "bowling_style": get_next_value_by_label(soup, "Bowling Style"),
        "teams": get_next_value_by_label(soup, "Teams"),
    }

    # Country fallback from visible header. For this page it appears near name.
    # Keep conservative for now.
    if " India " in f" {body_text} ":
        profile["country"] = "India"

    return profile


def main():
    r = requests.get(URL, headers=HEADERS, timeout=30)
    print("Status:", r.status_code)
    print("Length:", len(r.text))
    r.raise_for_status()

    profile = parse_profile(r.text, URL)

    print(json.dumps(profile, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
    