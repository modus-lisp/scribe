#!/usr/bin/env python3
"""ttx-oracle.py — vendor ground-truth sfnt table fields for scribe's table gates.

Reference (fontTools) is used in the HARNESS ONLY, never in scribe. Run once;
the emitted TSVs are the offline oracle the gates diff against.

  python3 inspect/ttx-oracle.py
  -> inspect/vectors/tables/<font>.tsv   (lines: "table.field<TAB>value")
"""
import os, sys
from fontTools.ttLib import TTFont

HERE = os.path.dirname(os.path.abspath(__file__))
CORPUS = os.path.join(HERE, "corpus")
OUT = os.path.join(HERE, "vectors", "tables")
os.makedirs(OUT, exist_ok=True)
HMTX_LIMIT = 300  # cap per-glyph rows; enough to validate the parser


def emit(font, w):
    head = font["head"]
    for f in ("unitsPerEm", "indexToLocFormat", "xMin", "yMin", "xMax", "yMax",
              "macStyle", "lowestRecPPEM", "flags", "magicNumber"):
        w(f"head.{f}\t{getattr(head, f)}")

    w(f"maxp.numGlyphs\t{font['maxp'].numGlyphs}")

    hhea = font["hhea"]
    for f in ("ascent", "descent", "lineGap", "advanceWidthMax",
              "minLeftSideBearing", "minRightSideBearing", "xMaxExtent",
              "numberOfHMetrics"):
        w(f"hhea.{f}\t{getattr(hhea, f)}")

    order = font.getGlyphOrder()
    metrics = font["hmtx"].metrics
    for gid in range(min(len(order), HMTX_LIMIT)):
        adv, lsb = metrics[order[gid]]
        w(f"hmtx.advance.{gid}\t{adv}")
        w(f"hmtx.lsb.{gid}\t{lsb}")

    if "OS/2" in font:
        os2 = font["OS/2"]
        for f in ("usWeightClass", "usWidthClass", "sTypoAscender",
                  "sTypoDescender", "sTypoLineGap", "sxHeight", "sCapHeight",
                  "xAvgCharWidth", "fsSelection", "usWinAscent", "usWinDescent"):
            if hasattr(os2, f):
                w(f"OS_2.{f}\t{getattr(os2, f)}")

    post = font["post"]
    for f in ("italicAngle", "underlinePosition", "underlineThickness",
              "isFixedPitch"):
        w(f"post.{f}\t{getattr(post, f)}")

    name = font["name"]
    for nid in (1, 2, 4, 6):
        rec = name.getName(nid, 3, 1, 0x409) or name.getName(nid, 1, 0, 0)
        if rec is not None:
            w(f"name.{nid}\t{rec.toUnicode()}")


def main():
    for fn in sorted(os.listdir(CORPUS)):
        if not fn.lower().endswith((".ttf", ".otf")):
            continue
        font = TTFont(os.path.join(CORPUS, fn))
        lines = []
        emit(font, lines.append)
        out = os.path.join(OUT, fn.rsplit(".", 1)[0] + ".tsv")
        with open(out, "w") as f:
            f.write("\n".join(lines) + "\n")
        print(f"{fn}: {len(lines)} fields -> {os.path.relpath(out, HERE)}")


if __name__ == "__main__":
    main()
