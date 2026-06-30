# Changelog

## 0.1.0 — initial public release
First-class text rendering, pure Common Lisp, clean-room:
- Containers: sfnt, WOFF, WOFF2 (glyf-transform reversal); own DEFLATE + brotli-pure.
- Outlines: TrueType glyf, CFF/CFF2 (Type2 + flex), composites.
- Variable fonts: fvar/avar/gvar/HVAR (= fontTools instancer).
- Shaping: GSUB single/multiple/ligature/contextual+chaining, GPOS single/pair/
  mark attachment, GDEF + lookupFlag (= HarfBuzz).
- Rasterizer: analytic signed-area coverage (quad+cubic) = FreeType.
- Compositing: gamma-correct linear-light blend, subpixel positioning, stem darkening.
