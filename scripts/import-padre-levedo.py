#!/usr/bin/env python3
"""Split the recovered Padre Levedo monthly archives into standalone posts."""

from __future__ import annotations

import argparse
import html
import re
import shutil
import unicodedata
from dataclasses import dataclass
from datetime import date
from pathlib import Path


MONTHS = {
    "janeiro": 1,
    "fevereiro": 2,
    "março": 3,
    "abril": 4,
    "maio": 5,
    "junho": 6,
    "julho": 7,
    "agosto": 8,
    "setembro": 9,
    "outubro": 10,
    "novembro": 11,
    "dezembro": 12,
}
ARCHIVED_SITE = (
    "https://web.archive.org/web/20070601000000im_/"
    "http://www.padrelevedo.hpg.ig.com.br/"
)

ENTRY_RE = re.compile(
    r'<div id="date">(?P<date>.*?)</div>\s*'
    r"<blockquote>(?P<body>.*?)</blockquote>\s*"
    r'<div class="comments">(?P<comments>.*?)</div>',
    re.IGNORECASE | re.DOTALL,
)
TAG_RE = re.compile(r"<[^>]+>")
LEADING_CLEAR_RE = re.compile(
    r'^\s*<div\s+style="clear:both;">\s*</div>', re.IGNORECASE
)
TRAILING_CLEAR_RE = re.compile(
    r'<div\s+style="clear:both;\s*padding-bottom:\s*0\.25em;">\s*</div>\s*$',
    re.IGNORECASE,
)


@dataclass
class Post:
    published: date
    title: str
    body: str
    filename: str = ""


def plain_text(fragment: str) -> str:
    return " ".join(html.unescape(TAG_RE.sub(" ", fragment)).split())


def parse_date(fragment: str) -> date | None:
    text = plain_text(fragment)
    if not text:
        return None

    match = re.search(
        r",\s*([A-Za-zÀ-ÿ]+)\s+(\d{1,2}),\s*(\d{4})$", text
    )
    if not match:
        raise ValueError(f"unrecognized date: {text!r}")

    month_name, day, year = match.groups()
    return date(int(year), MONTHS[month_name.casefold()], int(day))


def title_from_body(body: str) -> str:
    candidates = [
        r"<b>(.*?)</b>",
        r'<span[^>]*font-weight\s*:\s*bold[^>]*>(.*?)</span>',
        r"<strong>(.*?)</strong>",
    ]
    for pattern in candidates:
        match = re.search(pattern, body, re.IGNORECASE | re.DOTALL)
        if match:
            title = plain_text(match.group(1))
            if title:
                return title

    text = plain_text(body)
    if not text:
        return "Sem título"

    first_line = re.split(r"<br\s*/?>", body, maxsplit=1, flags=re.IGNORECASE)[0]
    title = plain_text(first_line) or text
    if len(title) > 80:
        title = title[:77].rstrip() + "..."
    return title


def slugify(value: str) -> str:
    ascii_value = (
        unicodedata.normalize("NFKD", value)
        .encode("ascii", "ignore")
        .decode("ascii")
        .lower()
    )
    slug = re.sub(r"[^a-z0-9]+", "-", ascii_value).strip("-")
    return slug[:70].rstrip("-") or "sem-titulo"


def normalize_archived_urls(fragment: str) -> str:
    fragment = re.sub(
        r'(?P<prefix>\b(?:href|src)=["\'])/web/',
        r"\g<prefix>https://web.archive.org/web/",
        fragment,
        flags=re.IGNORECASE,
    )
    return re.sub(
        r'(?P<prefix>\bsrc=["\'])(?P<path>(?![a-z]+:|/|#)[^"\']+)',
        lambda match: (
            match.group("prefix") + ARCHIVED_SITE + match.group("path")
        ),
        fragment,
        flags=re.IGNORECASE,
    )


def read_posts(source: Path) -> list[Post]:
    posts: list[Post] = []
    for archive in sorted(source.glob("*_archive.html")):
        archive_html = archive.read_text(encoding="utf-8")
        matches = list(ENTRY_RE.finditer(archive_html))
        block_count = len(
            re.findall(r"<blockquote>", archive_html, re.IGNORECASE)
        )
        if len(matches) != block_count:
            raise ValueError(
                f"{archive}: extracted {len(matches)} of {block_count} posts"
            )

        current_date: date | None = None
        for match in matches:
            explicit_date = parse_date(match.group("date"))
            current_date = explicit_date or current_date
            if current_date is None:
                raise ValueError(f"{archive}: first post has no date")

            body = LEADING_CLEAR_RE.sub("", match.group("body"))
            body = TRAILING_CLEAR_RE.sub("", body)
            body = normalize_archived_urls(body.strip())
            posts.append(Post(current_date, title_from_body(body), body))

    assign_filenames(posts)
    return sorted(posts, key=lambda post: (post.published, post.filename), reverse=True)


def assign_filenames(posts: list[Post]) -> None:
    used: set[str] = set()
    for post in posts:
        stem = f"{post.published.isoformat()}-{slugify(post.title)}"
        filename = f"{stem}.html"
        suffix = 2
        while filename in used:
            filename = f"{stem}-{suffix}.html"
            suffix += 1
        post.filename = filename
        used.add(filename)


def page(title: str, body: str, description: str = "") -> str:
    description_meta = (
        f'\n  <meta name="description" content="{html.escape(description, quote=True)}">'
        if description
        else ""
    )
    return f"""<!doctype html>
<html lang="pt-BR">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{html.escape(title)} - Padre Levedo</title>{description_meta}
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <main>
{body}
  </main>
</body>
</html>
"""


def render_post(post: Post, newer: Post | None, older: Post | None) -> str:
    links = ['<a href="index.html">Todos os textos</a>']
    if newer:
        links.append(
            f'<a href="{html.escape(newer.filename, quote=True)}">Texto mais recente</a>'
        )
    if older:
        links.append(
            f'<a href="{html.escape(older.filename, quote=True)}">Texto mais antigo</a>'
        )

    body = f"""    <nav>{" · ".join(links)}</nav>
    <article>
      <header>
        <h1>{html.escape(post.title)}</h1>
        <time datetime="{post.published.isoformat()}">{post.published.strftime("%d/%m/%Y")}</time>
      </header>
      <div class="post-content">
        {post.body}
      </div>
    </article>
    <nav>{" · ".join(links)}</nav>"""
    return page(post.title, body, plain_text(post.body)[:160])


def render_index(posts: list[Post]) -> str:
    items = "\n".join(
        "      <li>"
        f'<time datetime="{post.published.isoformat()}">'
        f'{post.published.strftime("%d/%m/%Y")}</time> '
        f'<a href="{html.escape(post.filename, quote=True)}">'
        f"{html.escape(post.title)}</a></li>"
        for post in posts
    )
    body = f"""    <header>
      <h1>Padre Levedo</h1>
      <p>Um resgate do blog deste santíssimo indivíduo.</p>
    </header>
    <ol class="post-index">
{items}
    </ol>"""
    return page("Arquivo", body, "Arquivo recuperado do blog Padre Levedo.")


STYLE = """\
:root {
  color-scheme: light dark;
  font-family: Georgia, "Times New Roman", serif;
  line-height: 1.6;
}

body {
  margin: 0;
}

main {
  max-width: 48rem;
  margin: 0 auto;
  padding: 2rem 1.25rem 4rem;
}

a {
  color: #865b22;
}

nav {
  margin: 1rem 0 2rem;
}

article header {
  margin-bottom: 2rem;
}

h1 {
  line-height: 1.2;
}

time {
  white-space: nowrap;
}

.post-index {
  padding-left: 1.5rem;
}

.post-index li {
  margin: 0.35rem 0;
}

.post-index time {
  display: inline-block;
  width: 6.5rem;
}

.post-content img {
  max-width: 100%;
  height: auto;
}

@media (prefers-color-scheme: dark) {
  a {
    color: #e0ad66;
  }
}
"""


def write_site(posts: list[Post], output: Path) -> None:
    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)

    (output / "index.html").write_text(render_index(posts), encoding="utf-8")
    (output / "style.css").write_text(STYLE, encoding="utf-8")
    for index, post in enumerate(posts):
        newer = posts[index - 1] if index > 0 else None
        older = posts[index + 1] if index + 1 < len(posts) else None
        (output / post.filename).write_text(
            render_post(post, newer, older), encoding="utf-8"
        )


def main() -> None:
    repository = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "source",
        nargs="?",
        type=Path,
        default=repository.parent / "xyah.github.io",
    )
    parser.add_argument(
        "output",
        nargs="?",
        type=Path,
        default=repository / "site" / "padre-levedo",
    )
    args = parser.parse_args()

    posts = read_posts(args.source)
    write_site(posts, args.output)
    print(f"Wrote {len(posts)} posts to {args.output}")


if __name__ == "__main__":
    main()
