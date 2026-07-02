# TrueType hinting for scribe — plan

## Why
scribe's rasterizer is analytic-coverage (exact area) — optimal AA, the limit of
supersampling. But it rasterizes glyph outlines *geometrically*, without
grid-fitting. At small ppem (~7–11px) glyph features (stems, baseline, x-height)
land on fractional pixel rows, so stems render as ~0.7px grey smears and the
baseline appears to wobble. Large sizes are fine; small body text (e.g. HN's 7pt
metadata) reads sloppy. Supersampling can't fix this (accuracy isn't the problem,
grid-fitting is); a HiDPI (2×) output would, but the target is **1× fidelity**.

The fix is **hinting**: grid-fit the outline before rasterizing.

## Approach: interpret the font's own TrueType bytecode
Chosen over an autohinter or lightweight vertical-snap because:
- The vendored fonts ship real hints (Liberation & DejaVu both have
  `fpgm`/`prep`/`cvt`) — designer-intended, high quality.
- It's **differentially verifiable against FreeType at the point level**, which is
  scribe's whole methodology (shaping vs HarfBuzz, cmap vs fontTools).
- Target: FreeType's **classic interpreter (v35)** — full x+y grid-fit, grayscale
  smooth mode (not ClearType/v40 y-only).

## Where it slots in
Today: `glyph-outline` parses points → segments (font units, **instructions
skipped**, glyf.lisp:28); `rasterize-glyph` scales by `ppem/upem`.
New: **raw points + instructions → interpreter grid-fits points (F26Dot6 pixel
units) → segments → rasterize.** gvar already moves points pre-raster (precedent).
Arithmetic is **26.6 / 2.14 fixed point** to match FreeType bit-for-bit.

## Oracle (Phase 0 — DONE)
`inspect/hint-oracle.py <font> <ppem> <chars>` — freetype-py, native bytecode
hinter (`FT_LOAD_NO_AUTOHINT`), interpreter pinned to v35, dumps the **hinted**
`FT_Outline` points in 26.6 per contour + hinted advance. Ground truth for every
opcode. (Verified: `l`@12px → stem y 0.00→9.00, ~1px wide.)

## Phases
- **P0 Oracle & harness** — done. Point-level compare of scribe-hinted vs FT.
- **P1 Tables** — parse `cvt `, `fpgm`, `prep`, per-glyph instructions (capture at
  glyf.lisp:28), maxp v1 interpreter limits (maxStack/Storage/FunctionDefs/
  Twilight/…), `gasp` (size→policy).
- **P2 Interpreter VM** (bulk) — graphics state, stack, storage, scaled CVT,
  function defs; F26Dot6/F2Dot14 math; projection/freedom-vector move; the
  rounding engine (RTG/RTHG/RTDG/ROFF/SROUND/S45ROUND + engine compensation);
  ~130 opcodes grouped: push/arith/logic, GS setters, rounding, point-movers
  (MDAP/MIAP/MDRP/MIRP/IP/IUP/SHP/SHPIX/ALIGNRP/ISECT), CVT r/w, flow
  (IF/CALL/FDEF/LOOPCALL/JMPR), DELTAP/DELTAC, measurement (GC/MD/MPPEM/GETINFO).
- **P3 Glyph pipeline** — phantom points (4), twilight+glyph zones, original vs
  current coords, run prep-per-ppem then glyph program, final IUP.
- **P4 Composites** — component placement, ROUND_XY_TO_GRID, USE_MY_METRICS,
  composite instructions.
- **P5 Integration** — `*hinting*` mode + `gasp` ppem policy; cache hinted glyph
  per (gid, ppem, variation); grayscale target; gvar deltas before hinting;
  likely dial back weft's gamma/stem-darkening once stems are solid.
- **P6 Validation gate** — all vendored fonts × ppem 7–16 × ASCII+Latin-1,
  point-level + pixel-level vs FreeType, wired into `inspect/run-all.sh`.

## Milestones
- **M1 (proof / de-risk):** P0 oracle + interpreter skeleton + `fpgm`/`prep` +
  a few glyphs (`l i H o`) matching FreeType's hinted points at one ppem. Proves
  the fixed-point core + VM on real data before committing to the full opcode set.
- **M2:** full ASCII point-exact at ppem 7–12.
- **M3:** composites + DELTA + sweep gate + weft wiring (`*hinting*` on → the 7pt
  "4 hours" line goes crisp at 1×).

## Risks
Bit-exact fixed-point rounding (engine compensation, SROUND); DELTA exceptions
(per-ppem tweaks, needed for exact small-size match); composites; performance
(→ per-(gid,ppem) cache). Grayscale smooth only, not ClearType.

## Status (banked checkpoint)
- **P0 oracle** — done (`inspect/hint-oracle.py`, v35-pinned point ground truth).
- **P1 tables** — done (maxp1, cvt, fpgm/prep/glyph instructions).
- **P2 interpreter core** — done; runs LiberationSans `fpgm` (71 fns) + `prep` clean.
- **P3 glyph pipeline + movers** — done; MDAP/MIAP/MDRP/MIRP/IP/IUP/SHP/GC/MD/SCFS
  + phantom points.
- **M1 proof — ACHIEVED**: `l` @12px is **bit-exact** vs FreeType
  `(0.91,0)(0.91,9)(1.95,9)(1.95,0)`.  Fixed 3 fixed-point bugs (muldiv divisor
  sign; hround(0); phantom grid-fit).
- **M1 remaining**: `i/H/o/…` within ~1px.  Root cause isolated: **cap/x-height
  blue-zone snapping** — FT rounds `H`'s cap (funit 1409 → 8.25px) UP to 9 via the
  alignment zones; our MDAP path rounds to nearest (8).  All x-hinting is already
  exact.  Next: crack the blue-zone rounding (likely fixes i/H/o/n/e together),
  then M2/M3 per above.
- Comparison harness: `/tmp/weftdump.lisp` + oracle diff (per-point, any glyphs).
