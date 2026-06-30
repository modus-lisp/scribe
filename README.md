# scribe

**First-class text rendering in pure Common Lisp.** Clean-room — no FFI, no
FreeType, no HarfBuzz, no Cairo. A scribe writes letterforms by hand.

scribe is the text/font engine for [`weft`](../weft) (a pure-CL web engine), but
it stands alone and is useful to anything that needs to put excellent text on a
surface. The bar is not "draws glyphs" — it is the irrationally-good end of the
spectrum: correct shaping (ligatures, real kerning), variable fonts, and the
gamma/subpixel craft that separates "homemade" from wezterm/macOS-grade text.

## Why this exists

There is no pure-CL stack that reaches this bar. `zpb-ttf` parses TrueType and
`cl-vectors`/`cl-aa` anti-aliases an outline — solid, and a fine prototyping
scaffold/oracle — but together they cover only the *least* differentiating ~30%:
no CFF/CFF2, no variable fonts, no GSUB/GPOS shaping, no WOFF2, no hinting, and
none of the gamma/subpixel/stem-darkening tuning that actually makes text look
first-class. Those gaps are exactly the taste-critical parts, so scribe owns them.

## The pipeline

```
font bytes ──parse──▶ glyph outlines + metrics + OT-Layout tables   (font.lisp)
   text    ──shape──▶ positioned glyphs: GSUB ligatures, GPOS kerning (shape.lisp)
 outline   ──raster─▶ analytic coverage mask, subpixel-positioned     (raster.lisp)
coverage   ──blend──▶ gamma-correct linear-light compositing          (blend.lisp ✅)
```

### The quality levers (where "irrationally good" is won)

1. **Linear-light compositing** — coverage blended in linear, not sRGB. The most
   common homemade mistake; makes anti-aliased stems look right instead of muddy.
   **Done** (`blend.lisp`). See `demo/gamma-demo.lisp`.
2. **Analytic-coverage rasterization** — exact signed-area coverage, not
   supersampling (FreeType-smooth / stb lineage).
3. **Subpixel positioning** — fractional pen origin, cached into N subpixel
   buckets, so spacing isn't snapped to whole pixels (the wezterm evenness).
4. **Stem darkening / gamma tuning** — keep thin strokes from disappearing.
5. **Real shaping** — GSUB (programming ligatures `=> != >>=`, contextual
   alternates) + GPOS (modern kerning lives here, not the legacy `kern` table).
6. **Variable fonts** — interpolate outlines/metrics across weight/optical axes.
7. Later: hinting, LCD subpixel, BiDi/complex scripts, COLR v1 / emoji.

## Status

- **`blend.lisp` ✅** — gamma-correct linear-light coverage compositing, a
  zero-dependency RGB8 canvas, and a minimal PNG writer (stored deflate). Run
  `sbcl --script demo/gamma-demo.lisp` → `/tmp/scribe-gamma.png`: a side-by-side
  proof that the naive sRGB blend renders coverage too dark and the linear blend
  is correct.
- **`font.lisp` / `raster.lisp` / `shape.lisp`** — scaffolded with contracts;
  build order documented in each file's header.

## Build order (optimized for Latin/code fonts at HiDPI first)

1. linear-light compositing — **done**
2. analytic-coverage rasterizer (quad + cubic) — **done**
3. TrueType parse (`cmap`/`glyf`/`loca`/`hmtx`) → real glyphs, subpixel-positioned — **done**
4. CFF/CFF2 (cubic charstrings, flex hints) — **done** (non-CID)
5. GPOS kerning + GSUB ligatures — **done** (byte-identical to HarfBuzz on the
   test corpus; remaining lookup *formats* are the W4 swarm surface)
6. variable fonts (`fvar`/`gvar`) — next
7. WOFF2 (via `brotli-pure`)
8. the long tail: BiDi, complex shaping, COLR/emoji, hinting, LCD subpixel

## Differential oracle

scribe is tested the way `weft`/`zstd-pure`/`brotli-pure` are: against a
reference. FreeType (coverage bitmaps) and HarfBuzz (shaped glyph/position
streams) are the oracles — render/shape the same input both ways and diff. The
reference is used **only in the test harness** (`inspect/`), never in scribe
itself. Table-extraction grunt work parallelizes across cheap-model agents.

## Integrating into weft

weft consumes a thin interface and keeps its 7×13 bitmap as fallback:
`open-font` · `shape-run` · `rasterize-glyph` · `blend-coverage`. Zero coupling;
swap scribe in per-glyph when ready.

## License

MIT. Research / educational; not audited.
