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
- **M1 — CLOSED** on `l` bit-exact (the proof).  `i/H/o/…` are within ~1px; that
  gap is the first task of M2.
- Comparison harness: `/tmp/weftdump.lisp` + oracle diff (per-point, any glyphs).

## M2 — COMPLETE (round 3): full ASCII bit-exact on both fonts
Using the per-instruction FreeType trace (`inspect/hint-fttrace.md`), every residual
was root-caused and fixed.  **Printable ASCII (94 glyphs) is now 100% bit-exact vs
FreeType at ppem 7–16 for BOTH LiberationSans and DejaVuSans.**  LiberationSans
accented Latin-1 (53 composites) is also 53/53 exact.

The round-3 fixes (all found via the STATE trace):
- **`FT_Vector_NormLen`** exact port for SPVTL/SFVTL normalization (isqrt was ~9/16384
  off; projection vectors now bit-exact).
- **`dot14` rounds once** (FreeType TT_DotFix14: sum then round), not per-term.
- **Distances project the DIFFERENCE vector** — MDRP/MIRP/IP/MD/ALIGNRP/MSIRP/SHP all
  compute PROJECT(cur[p]−cur[q]) / DUALPROJ(org[p]−org[q]) once, not project(p)−
  project(q).  This alone took DejaVu 71→90.
- **IUP hybrid**: boundary classification uses scaled `org`, interior ratio uses
  `orus` (font units) via DivFix→MulFix — exact port of `iup_worker_interpolate_`.
  Fixed the LiberationSans b/g/%/& 1/64 tail.
- **MIRP order**: auto-flip cvt *before* the cut-in test (fixed `%`@16).
- **`orus-dist`** dual-projects then scales (MulFix(DUALPROJ,scale)).
- **F_dot_P clamp**: near-perpendicular freedom/projection clamps to 0x4000 and still
  moves (fixed DejaVu `$`@7).
- **DIV opcode** was computing MUL (`a*b/64` instead of `a*64/b`) — corrupted fpgm
  arithmetic; fixed the LiberationSans accent-alignment functions → composites 53/53.

Remaining: DejaVuSans accented Latin-1 (~41–48/53) — the tilde/ring accents shear a
few 1/64 in their composite instructions (a DejaVu-specific residual, still under the
same trace method).

## M2 round-2 open items (superseded — kept for history)
Three residuals remained after round 2 (all fixed in round 3 above):
- **DejaVu diagonals (/, \, A, w, y, …).** `/`'s first diagonal MIRP (proj set by
  SPVTL parallel to the stroke ⇒ near-vertical; freedom = x) targets `dist=round(cvt
  [26]=64)` while the point already sits at the original diagonal distance `org=18`;
  the cut-in can't fire (font sets SCVTCI=640, and 46<640), so weft moves pt2 by 46
  projected → ×(16384/4734)=×3.46 along x → +159px, then the *next* SPVTL reads the
  now-degenerate edge and the cascade blows up (pt0 → −937 vs FT 189).  Verified NOT
  the cause: post-prep CVT is bit-exact vs FreeType (read from TT_Size); MDRP/MIRP
  round/min bits; norm14 precision; SPVTL dual = proj vs org.  The contradiction —
  identical inputs but FT keeps pt2≈67 (tiny move) — means FT's `dist` here is `org`
  (18), i.e. its cut-in *did* fire or its projection is near-horizontal; resolving it
  needs FreeType's live interpreter trace (freetype-py exposes none).  NB: forcing
  SFVTCA the wrong way (0x05→freedom-y) drops `/` 17.6→0.75px but is spec-incorrect
  (FreeType src: opcode&1=1 ⇒ x) — a red herring that only dodges the amplification.
- **LiberationSans 1/64 tail (b, g, ", …).** e.g. `b` pt6 is IUP[x]-interpolated
  between touched pt5 (orus 318, cur 121) and pt13 (orus 138, cur 54); orus 316 ⇒
  54 + MulDiv(178,67,180)=54+66=120, but FT=121.  Every formula (single MulDiv,
  DivFix+MulFix, either base) gives 120 with these inputs, so FT must touch pt6 or
  bracket it differently — unreproducible without FT's touch state.
- **Composite accents (é, î, ç @12; 29/52 exact).** Bases always exact; the accent
  is placed by an fpgm align-fn (CALL) that GC's the accent's own hinted points and
  MSIRPs the anchor.  For é the fn computes a shift that rounds to 0 (FT: +64) — the
  measured accent coordinate is 1px off, i.e. downstream of the same sub-pixel IUP
  tail above.  Fixing the tail should lift the accents too.

## M2 — SUBSTANTIALLY DONE (full ASCII point-exact)
The cap/x-height diagnosis was half-right (it *was* a prep bug) but the actual root
causes were opcode/semantics bugs, found by reading FreeType's post-prep CVT out of
`TT_Size` via `/proc/self/mem` (freetype-py doesn't expose it).  Fixes, each
oracle-gated:
- **RUTG** (round-up-to-grid) used super-round threshold 64; ceil's threshold is
  `period-1 = 63`, so `round_up(0)` returned 64 (a pixel gained).  Broke the cap/
  x-height CVTs computed in prep's fpgm helper.
- **DELTAC/DELTAP pair order**: the cvt/point INDEX is nearer the stack top than the
  exception spec; we popped them swapped, so every per-ppem exception hit the wrong
  entry / never matched → cap-height & x-height CVT deltas never fired at 12px.
  (Post-prep CVT now bit-exact vs FreeType.)
- **MDRP/MIRP round vs min-distance bits**: bit2 (0x04)=round, bit3 (0x08)=min-dist;
  we had them swapped.
- **IP + IUP interior use `orus`** (original font-unit coords), like FreeType, so the
  interpolation ratio keeps sub-pixel precision (scaled `org` loses it); IUP bases on
  the smaller-orus reference.
- **MDRP/MIRP original distance from `orus`** (scale the font-unit delta once) instead
  of subtracting two independently-scaled coords (double-round).
- **Per-glyph GS reset**: vectors/rp/zp/loop reset to the fixed x-axis default every
  glyph (policy fields — round mode, cut-ins, delta — restored from the post-prep
  snapshot).  Prep here leaves the vectors on the y axis, so glyphs that DELTAP the
  advance before any SVTCA (v/M/A/w/…) were moving it in y.
- **SPVTCA/SFVTCA axis bit** (0x02–0x05) was inverted.
- Opcodes added: **SHPIX, MSIRP, SPVTL/SFVTL, ISECT, SHC/SHZ**.

**Exact counts (printable ASCII, 94 glyphs), LiberationSans:** ppem7 87, 8 86, 9 87,
10 83, 11 89, 12 90, 14 91, 16 90.  Every remaining miss is a single 1/64px (0.016px)
interpolation-rounding edge case (leading hypothesis: a touched-vs-untouched
classification difference in a couple of curve control points).

**P4 composites — implemented**: components hinted standalone then transformed +
placed (ROUND_XY_TO_GRID) and the composite's own instructions run.  Bases always
exact; accented Latin-1 29/52 exact @12px (some accents 1px off — the standalone
component lsb/advance shift bleeds into the composite offset; refinement pending).

**Second font — DejaVuSans**: ~63–65/94 @ppem7–16.  Non-diagonal glyphs largely
exact; diagonal glyphs (/ \ A w y …) that drive a projection vector through twilight
points + SPVTL are still off (the SPVTL/twilight cascade yields a near-vertical
projection that amplifies MIRP moves) — the leading open item.

## M2 — original opening task (sharp diagnosis, kept for history)
Full ASCII exact is gated on one thing: **the cap/x-height CVT after `prep`.**
`H`'s cap is MIAP'd to **cvt[3] = 528 F26.6 (8.25px)**; with cur=528 and no
cut-in, MIAP rounds it to **8**, but FreeType gives **9** — so FreeType's cvt[3]
at 12px differs from ours.  This is a **`prep` correctness** issue (the alignment-
zone CVT adjustment / a fpgm function), NOT a mover bug — all x-hinting is already
bit-exact, and MIAP/MDAP/round are correct given the CVT.  Next step: dump
FreeType's post-`prep` CVT (freetype-py exposes it) and diff against ours at
ppem 12 to find which entries `prep` computes differently, then fix the fpgm/prep
opcode(s) responsible.  Likely fixes `i/H/o/n/e/a/…` as a group (shared cap/x-
height zones).  Then composites (P4), the validation gate (P6), and weft wiring.
