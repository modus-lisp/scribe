;;;; shape.lisp — OpenType Layout shaping (GSUB/GPOS).  [SCAFFOLD — contracts]
;;;;
;;;; The HarfBuzz-class problem and the biggest "draws letters" vs "first-class"
;;;; divider.  Turns a run of Unicode + features into positioned glyphs.
;;;;
;;;; Build order:
;;;;   1. Segmentation: grapheme clusters (UAX #29), script/direction runs.
;;;;   2. cmap mapping + GDEF glyph classes.
;;;;   3. GSUB: ligatures (liga), contextual alternates (calt), stylistic sets —
;;;;      this is what gives programming ligatures (=> != >>=) and is table
;;;;      stakes for code-font users.
;;;;   4. GPOS: pair/class kerning (modern kerning lives here, not `kern`),
;;;;      mark-to-base / mark-to-mark attachment, cursive joining.
;;;;   5. Complex scripts later: Arabic joining, Indic/USE reordering, BiDi.
;;;;
;;;; Contract: shape-run returns a vector of glyph records:
;;;;   (gid x-advance y-advance x-offset y-offset cluster)
;;;; in font units; layout converts to pixels with the rasterizer's scale.
(in-package #:scribe)

(defstruct glyph-pos gid x-advance y-advance x-offset y-offset cluster)

(defun shape-run (font text &key (features '(:liga :calt :kern))
                                 (script :latn) (direction :ltr) variation)
  "Shape TEXT (a string) with FONT into positioned glyphs applying FEATURES.
   [not yet implemented — shape.lisp milestone 3/4]"
  (declare (ignore font text features script direction variation))
  (error "scribe: shape-run not yet implemented"))
