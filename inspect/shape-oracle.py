#!/usr/bin/env python3
"""shape-oracle.py — HarfBuzz ground truth for scribe's shaper (harness only).

Emits, per (font, test-string), the shaped glyph + position stream:
  <gid> <x_advance> <x_offset>   (font units, one line per output glyph)
Sections are headed by  "# <fontstem> | <string>".
  python3 inspect/shape-oracle.py > inspect/vectors/shape/expected.txt
"""
import os, glob, sys
import uharfbuzz as hb

HERE = os.path.dirname(os.path.abspath(__file__))
TESTS = ["AVATAR", "Wave To Yo.", "office fluffy waffle", "VA Ta We Yo",
         "The quick brown fox.", "affluent affix", "1/2 = .5"]

def shape(font_path, text, features):
    blob = hb.Blob.from_file_path(font_path)
    face = hb.Face(blob); fnt = hb.Font(face)
    buf = hb.Buffer(); buf.add_str(text)
    buf.guess_segment_properties()
    hb.shape(fnt, buf, features)
    out = []
    for info, pos in zip(buf.glyph_infos, buf.glyph_positions):
        out.append((info.codepoint, pos.x_advance, pos.x_offset))
    return out

def main():
    feats = {"kern": True, "liga": True, "calt": True}
    for fp in sorted(glob.glob(os.path.join(HERE, "corpus", "*.ttf")) +
                     glob.glob(os.path.join(HERE, "corpus", "*.otf"))):
        stem = os.path.splitext(os.path.basename(fp))[0]
        for t in TESTS:
            print(f"# {stem} | {t}")
            for gid, xa, xo in shape(fp, t, feats):
                print(f"{gid} {xa} {xo}")

if __name__ == "__main__":
    main()
