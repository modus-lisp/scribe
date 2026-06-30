# scribe

**First-class text rendering in pure Common Lisp.** Clean-room — no FFI, no
FreeType, no HarfBuzz, no Cairo. A scribe writes letterforms by hand.

scribe takes font bytes and a string and produces correctly shaped, antialiased,
gamma-correct pixels — the whole pipeline, from `wOF2`/sfnt parsing through
OpenType shaping to an analytic rasterizer — implemented from scratch. It is the
text engine for [`weft`](https://github.com/modus-lisp) (a pure-CL web engine)
but stands alone. The bar is not "draws glyphs"; it is matching the reference
implementations byte-for-byte.

## What works

| Stage | Coverage |
|---|---|
| **Containers** | sfnt (`.ttf`/`.otf`), **WOFF**, **WOFF2** (glyf-transform reversal) |
| **Outlines** | TrueType `glyf` (quadratic), **CFF/CFF2** (cubic Type2 charstrings + flex), composite glyphs |
| **Variable fonts** | `fvar` axes, `avar` remap, `gvar` deltas, **`HVAR`** advance variations |
| **Shaping (GSUB)** | single, multiple, ligature, contextual + **chaining-context** (`calt`/`ccmp`) |
| **Shaping (GPOS)** | single, pair (**kerning**), **mark-to-base/mark/ligature** (accents) |
| | + GDEF classes & `lookupFlag` mark filtering, extension lookups |
| **Rasterizer** | analytic signed-area coverage (not supersampling), quad + cubic |
| **Compositing** | **gamma-correct** linear-light blend, **subpixel** positioning, stem-darkening knob |

Everything is verified against the reference implementations — and matches them:

- **Rasterization** — coverage matches **FreeType** (ink within 0.5%; visually
  identical). Measured against Apple's own baked text, scribe's AA profile is the
  same: grayscale, geometric, ~1px edges, no subpixel (the Safari/Retina recipe).
- **Shaping** — **byte-identical to HarfBuzz** across the corpus (kerning,
  ligatures, contextual/`calt`, small-caps, accents): 330/330 + 170/170 glyph
  positions, gid streams exact.
- **Variable fonts** — advance & outline deltas match the **fontTools instancer**
  exactly (e.g. New York `H` = 1446 / 1662 / 1735 at default / opsz12 / wght1000).
- **WOFF/WOFF2** — reconstructed outlines are identical to the originals
  (WOFF1 3369/3369, WOFF2-glyf 3369/3369 simple+composite, WOFF2-CFF 11/11).

## Quick start

```lisp
(asdf:load-system "scribe")

(let* ((font  (scribe:open-font (alexandria:read-file-into-byte-vector "Font.ttf")))
       (glyphs (scribe:shape-run font "Hard waffle —fi"        ; cmap + GSUB + GPOS
                                 :features '(:liga :kern :calt))))
  ;; glyphs: a vector of positioned glyph-pos (gid + advances/offsets, font units)
  (loop for g across glyphs
        do (multiple-value-bind (coverage w h left top)
               (scribe:rasterize-glyph font (scribe::glyph-pos-gid g) 48)
             ;; coverage: (simple-array double-float) of w*h in [0,1]
             (scribe:blend-coverage canvas x y cov '(0 0 0)))))   ; gamma-correct
```

`open-font` transparently accepts `.ttf`, `.otf`, `.woff`, and `.woff2`. See
`demo/` for runnable specimens (`gamma-demo`, `text-demo`, `cff-demo`,
`shape-demo`, `var-demo`, `hidpi-demo`) — each writes a PNG to `/tmp`.

## Building

```sh
git clone --recursive https://github.com/modus-lisp/scribe.git
cd scribe
sh inspect/run-all.sh        # the offline gate suite (no Python needed)
```

`--recursive` pulls the one dependency, [`brotli-pure`](https://github.com/modus-lisp/brotli-pure)
(a from-scratch Common Lisp Brotli codec), needed only for WOFF2. WOFF1 and the
rest use scribe's own from-scratch DEFLATE (`src/deflate.lisp`).

## Design

- **No hinting, no subpixel** — both are deliberate. Hinting's grid-snap creates
  the jagged look; subpixel AA is panel-dependent and can't be baked into a
  portable image (Apple dropped it in 2018). scribe bets on grayscale analytic AA
  + stem darkening + rendering at the device's native DPI — the modern,
  display-independent path.
- **Differential everything** — every layer is gated against a reference
  (FreeType for coverage, HarfBuzz for shaping, the fontTools instancer for
  variations), the same discipline as `brotli-pure`/`zstd-pure`. References live
  **only in the test harness** (`inspect/`), never in scribe. The vendored oracle
  vectors let the gates run offline.

## Layout

```
src/    blend (compositing+canvas+PNG) · font (sfnt) · tables/ · cmap · cff ·
        var (fvar/avar/gvar/HVAR) · glyf · raster · otl (GSUB/GPOS) · woff · deflate
inspect/  the gate suite + vendored oracle vectors + redistributable test fonts
demo/   runnable specimens
tools/  dmg-fonts (pure-CL Apple-font extractor), reusable deflate
```

## Status & license

Research / educational; not audited. MIT (`LICENSE`). Test-corpus fonts are
vendored under their own licenses — see `inspect/corpus/FONT-LICENSES.md`.
