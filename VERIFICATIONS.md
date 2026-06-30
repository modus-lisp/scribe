# Verifications

Every layer is gated against a reference implementation. References are used in
the test harness only (`inspect/`), never in scribe. Run `sh inspect/run-all.sh`
for the offline suite (no Python needed — it diffs vendored oracle vectors).

| Layer | Reference | Result |
|---|---|---|
| sfnt table fields (head/hhea/maxp/hmtx/OS_2/post/name) | fontTools `ttx` | exact |
| cmap (fmt 4/6/12) char→gid | fontTools `getBestCmap` | 17k+ mappings exact |
| glyf/CFF outlines | rendered shapes | correct (visual + composite) |
| analytic rasterizer | **FreeType** (unhinted coverage) | ink within 0.5%, visually identical |
| AA profile vs Apple's baked text | measured from apple.com asset | same: grayscale, geometric, ~1px edges, no subpixel |
| shaping GSUB+GPOS (kern/liga/calt/marks/smcp…) | **HarfBuzz** (`uharfbuzz`) | 330/330 + 170/170 positions; gid streams exact |
| variable fonts (fvar/avar/gvar/HVAR) | **fontTools instancer** | outline + advance deltas exact |
| WOFF1 | original font | 3369/3369 outlines identical |
| WOFF2 glyf (transform reversal) | original font | 3369/3369 (2048 simple + 1299 composite + 22 empty) |
| WOFF2 CFF | original font | 11/11 outlines identical |

Notes:
- New York's HarfBuzz *default* advances disagree with the fontTools instancer (a
  HarfBuzz default-coords quirk for that Apple variable font); scribe matches the
  instancer — the authoritative variable-font implementation. New York is not in
  the committed corpus (not redistributable), so the public gate uses only the
  free fonts, all green.
