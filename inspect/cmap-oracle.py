#!/usr/bin/env python3
"""cmap-oracle.py — vendor per-subtable codepoint->gid ground truth for W2.

For each corpus font and each cmap subtable of a format we test (4, 6, 12),
write inspect/vectors/cmap/<stem>.<pid>.<eid>.<fmt>.tsv with "cp<TAB>gid" lines
(decimal). gid via fontTools getGlyphID. Reference used in the harness only.
"""
import os, glob
from fontTools.ttLib import TTFont

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "vectors", "cmap")
os.makedirs(OUT, exist_ok=True)
FORMATS = {4, 6, 12}

def main():
    for fp in sorted(glob.glob(os.path.join(HERE, "corpus", "*.ttf"))):
        stem = os.path.splitext(os.path.basename(fp))[0]
        font = TTFont(fp)
        for s in font["cmap"].tables:
            if s.format not in FORMATS:
                continue
            name = f"{stem}.{s.platformID}.{s.platEncID}.{s.format}.tsv"
            with open(os.path.join(OUT, name), "w") as f:
                for cp, gname in sorted(s.cmap.items()):
                    f.write(f"{cp}\t{font.getGlyphID(gname)}\n")
            print(f"{name}: {len(s.cmap)} entries")

if __name__ == "__main__":
    main()
