;;;; font.lisp — sfnt / OpenType font parsing.  [SCAFFOLD — contracts only]
;;;;
;;;; Owns the data layer: parse the sfnt container + tables and expose glyph
;;;; outlines + metrics to the rasterizer and shaper.
;;;;
;;;; Build order (each a milestone):
;;;;   1. sfnt table directory; head, maxp, hhea, hmtx, OS/2, name, post.
;;;;   2. cmap (formats 4, 12, 14) — character -> glyph id.
;;;;   3. glyf/loca — TrueType QUADRATIC outlines (composite glyphs too).
;;;;   4. CFF/CFF2 — PostScript CUBIC outlines via a Type2 charstring VM.
;;;;   5. Variable fonts: fvar/avar axes, gvar/HVAR deltas (interpolate 3+4).
;;;;   6. WOFF/WOFF2 wrappers (WOFF2 decompresses via brotli-pure).
;;;;
;;;; A glyph outline is returned as a list of contours, each a list of segments:
;;;;   (:move x y) (:line x y) (:quad cx cy x y) (:cubic c1x c1y c2x c2y x y)
;;;; in FONT UNITS (font-units-per-em); the rasterizer scales to ppem.
(in-package #:scribe)

(defstruct font
  data            ; the raw (unsigned-byte 8) vector
  tables          ; tag -> (offset . length)
  units-per-em
  num-glyphs
  ascent descent line-gap
  cmap)           ; resolved char->gid map (filled by parse)

(defun open-font (bytes)
  "Parse an sfnt/OpenType font from BYTES into a FONT.  [not yet implemented]"
  (declare (ignore bytes))
  (error "scribe: open-font not yet implemented (font.lisp milestone 1)"))

(defun font-glyph-index (font codepoint)
  "Map a Unicode CODEPOINT to a glyph id via the font cmap. [not yet implemented]"
  (declare (ignore font codepoint))
  (error "scribe: font-glyph-index not yet implemented"))

(defun glyph-outline (font gid &key variation)
  "Return GID's outline as contours of segments in font units. [not yet implemented]"
  (declare (ignore font gid variation))
  (error "scribe: glyph-outline not yet implemented"))
