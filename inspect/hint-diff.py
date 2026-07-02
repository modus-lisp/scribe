#!/usr/bin/env python3
"""hint-diff — diff weft's hinted points against FreeType's, per glyph.
Usage: hint-diff.py <font.ttf> <ppem> <chars>"""
import sys, json, subprocess
font, ppem, chars = sys.argv[1], sys.argv[2], sys.argv[3]
w = {}
for line in subprocess.run(["sbcl","--non-interactive","--load","inspect/hint-weftdump.lisp",font,ppem,chars],
                           capture_output=True, text=True).stdout.splitlines():
    if line.startswith("PTS "):
        p = line[4:].split(); ch = p[0]
        w[ch] = None if (len(p) > 1 and p[1] == "NA") else [int(t) for t in p[1:]]
o = {}
oj = json.loads(subprocess.run(["python3","inspect/hint-oracle.py",font,ppem,chars],
                               capture_output=True, text=True).stdout)
for g in oj["glyphs"]:
    o[g["glyph"]] = [v for c in g["contours"] for x,y,on in c for v in (x,y)]
ex = tot = 0
print(f"=== {font.split('/')[-1]} @ {ppem}px ===")
for ch in chars:
    wf, of = w.get(ch), o.get(ch); tot += 1
    if wf is None or of is None: print(f"  '{ch}': skipped/composite"); continue
    if len(wf) != len(of): print(f"  '{ch}': COUNT weft {len(wf)} vs FT {len(of)}"); continue
    d = max((abs(a-b) for a,b in zip(wf, of)), default=0)
    if d == 0: ex += 1
    print(f"  '{ch}': {'EXACT' if d==0 else f'max Δ={d}/64 = {d/64:.3f}px'}")
print(f"exact: {ex}/{tot}")
