;;;; raster.lisp — analytic-coverage outline rasterizer.  [SCAFFOLD — contracts]
;;;;
;;;; Scan-converts a Bezier outline (from font.lisp, scaled to ppem) into an
;;;; exact per-pixel COVERAGE mask in [0,1] using signed-area accumulation —
;;;; the FreeType "smooth" / stb_truetype lineage, NOT supersampling.  The
;;;; coverage mask is then composited by blend.lisp in linear light.
;;;;
;;;; Quality levers that live here / just above here:
;;;;   - SUBPIXEL POSITIONING: rasterize at a fractional x-origin, cached into
;;;;     N horizontal subpixel buckets, so glyph spacing isn't snapped to whole
;;;;     pixels (the wezterm/macOS evenness).  raster takes a fractional offset.
;;;;   - STEM DARKENING / gamma already handled by blend.lisp; raster only owns
;;;;     geometric coverage.
;;;;   - (optional, later) LCD subpixel: rasterize at 3x horizontal + FIR filter.
;;;;   - (optional, later) hinting: grid-fit the outline before scan-conversion.
;;;;
;;;; Contract: rasterize-outline returns (values coverage-bitmap w h left top)
;;;; where coverage-bitmap is a (simple-array double-float) of w*h in [0,1] and
;;;; (left,top) is the bitmap origin relative to the pen, in pixels.
(in-package #:scribe)

(defun rasterize-outline (contours scale &key (dx 0d0) (dy 0d0))
  "Scan-convert CONTOURS (font units) at SCALE px/unit, fractional pen offset
   (DX,DY), to an analytic coverage bitmap. [not yet implemented]"
  (declare (ignore contours scale dx dy))
  (error "scribe: rasterize-outline not yet implemented (raster.lisp)"))

(defun rasterize-glyph (font gid ppem &key (subpixel 0d0) variation)
  "Convenience: outline -> scaled -> coverage bitmap for GID at PPEM.
   SUBPIXEL in [0,1) selects the horizontal subpixel bucket. [not yet implemented]"
  (declare (ignore font gid ppem subpixel variation))
  (error "scribe: rasterize-glyph not yet implemented"))
