;;;; packages.lisp — scribe package definitions
(defpackage #:scribe
  (:use #:cl)
  (:export
   ;; canvas + gamma-correct compositing (blend.lisp)
   #:canvas #:make-canvas #:canvas-width #:canvas-height #:canvas-pixels
   #:blend-coverage #:fill-coverage-span
   #:srgb->linear #:linear->srgb #:*srgb->linear* #:*linear->srgb*
   #:write-png
   ;; (later) font.lisp / shape.lisp / raster.lisp / atlas.lisp contracts
   #:open-font #:font-glyph-index #:font-units-per-em
   #:shape-run
   #:rasterize-glyph))
