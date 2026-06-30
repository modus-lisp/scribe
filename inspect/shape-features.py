#!/usr/bin/env python3
"""shape-features.py — HarfBuzz ground truth for scribe's OTL feature sweep.

For each (font, feature(s), test-string) case, emits the shaped glyph + full
position stream (gid x_advance y_advance x_offset y_offset, font units, one line
per output glyph). Sections are headed by:
    # <fontstem> | <feature-csv> | <string>
where <feature-csv> is the EXACT set of non-default features the case enables
(the always-on default set ccmp,locl,rlig,liga,calt,kern,mark,mkmk is implied and
also passed to HarfBuzz, matching how scribe shapes the default case).

  python3 inspect/shape-features.py > inspect/vectors/shape/features.txt

NOTE: NewYork.ttf is a variable font; HarfBuzz applies its HVAR advance-width
deltas at the default optical-size instance, adding a per-glyph constant to every
advance. scribe's base advance layer reads raw hmtx (no HVAR), so NewYork glyph
IDs match HarfBuzz exactly but advances differ by that HVAR delta. The gate's
gid-only column isolates substitution correctness (chain-context calt, single-sub
smcp/c2sc/...) from this out-of-OTL-scope metrics difference.
"""
import os, glob, sys
import uharfbuzz as hb

HERE = os.path.dirname(os.path.abspath(__file__))

# HarfBuzz default-on features for horizontal Latin (the set scribe also applies).
DEFAULT_ON = ["ccmp", "locl", "rlig", "liga", "calt", "kern", "mark", "mkmk"]

# (font-stem, [extra-features], test-string). Empty extra list => default shaping.
CASES = [
    # ---- GSUB single (1) ----
    ("NewYork",         ["smcp"],          "Hello World"),
    ("NewYork",         ["c2sc"],          "Hello"),
    ("NewYork",         ["tnum"],          "1234567890"),
    ("NewYork",         ["onum"],          "1234567890"),
    ("NewYork",         ["ss01"],          "figure"),
    ("DejaVuSans",      ["case"],          "Hello (Hi) [x]"),
    ("C059-Roman",      ["sups"],          "x2 1234"),
    ("C059-Roman",      ["numr"],          "12 34"),
    ("C059-Roman",      ["dnom"],          "12 34"),
    # ---- GSUB ligature (4) baseline (already done, keep as guard) ----
    ("DejaVuSans",      [],                "office waffle affix"),
    ("C059-Roman",      [],                "office fluffy"),
    # ---- GSUB multiple (2) + ccmp + ligature ----
    ("NewYork",         [],                "Hello World"),
    # ---- GPOS pair / kern baseline ----
    ("C059-Roman",      [],                "AVATAR Wave To Yo."),
    ("NewYork",         [],                "AVATAR To Yo Wa"),
    # ---- GSUB chaining context (6) calt ----
    ("NewYork",         [],                "1234567890"),    # calt+onum interplay default
    # ---- GSUB frac (ligature) / sups in C059 ----
    ("C059-Roman",      ["frac"],          "1 2 3"),
    ("C059-Roman",      ["ordn"],          "1o 2a No"),
    # ---- GPOS mark-to-base (4) : base + non-composing combining mark, so
    # HarfBuzz does not Unicode-compose (apples-to-apples vs scribe). Combining
    # U+0301 acute, U+0300 grave, U+0302 circ, U+0303 tilde, U+0308 diaeresis.
    ("DejaVuSans",      [],                'b́ d̀ f̂ h̃ k̈'),
    ("DejaVuSans",      [],                'q̀ v̂ x̃ z̈ t́'),
    ("DejaVuSansMono",  [],                'b́ d̂ f̈ h̃'),
    # ---- GPOS mark-to-mark (6) : two stacked non-composing marks ----
    ("DejaVuSans",      [],                'd́̈ t̃̇ b̂̀'),
    # ---- mixed text default ----
    ("DejaVuSans",      [],                "The quick brown fox."),
    ("DejaVuSansMono",  [],                "office -> waffle"),
]


def find_font(stem):
    for ext in (".ttf", ".otf"):
        p = os.path.join(HERE, "corpus", stem + ext)
        if os.path.exists(p):
            return p
    raise SystemExit("missing font: " + stem)


def shape(font_path, text, features):
    blob = hb.Blob.from_file_path(font_path)
    face = hb.Face(blob); fnt = hb.Font(face)
    buf = hb.Buffer(); buf.add_str(text)
    buf.guess_segment_properties()
    hb.shape(fnt, buf, features)
    out = []
    for info, pos in zip(buf.glyph_infos, buf.glyph_positions):
        out.append((info.codepoint, pos.x_advance, pos.y_advance,
                    pos.x_offset, pos.y_offset))
    return out


def main():
    for stem, extra, text in CASES:
        fp = find_font(stem)
        feats = {f: True for f in DEFAULT_ON}
        for f in extra:
            feats[f] = True
        csv = ",".join(extra)
        print(f"# {stem} | {csv} | {text}")
        for gid, xa, ya, xo, yo in shape(fp, text, feats):
            print(f"{gid} {xa} {ya} {xo} {yo}")


if __name__ == "__main__":
    main()
