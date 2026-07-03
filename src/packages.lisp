;;;; packages.lisp — scribe package definitions
(defpackage #:scribe
  (:use #:cl)
  (:export
   ;; canvas + gamma-correct compositing (blend.lisp)
   #:canvas #:make-canvas #:make-rgba-canvas #:canvas-width #:canvas-height
   #:canvas-pixels #:canvas-alpha
   #:blend-coverage #:fill-coverage-span
   #:srgb->linear #:linear->srgb #:*srgb->linear* #:*linear->srgb*
   #:write-png
   ;; (later) font.lisp / shape.lisp / raster.lisp / atlas.lisp contracts
   #:open-font #:font-glyph-index #:font-units-per-em
   #:shape-run
   #:rasterize-glyph #:rasterize-outline
   ;; font matching + metric accessors (match.lisp)
   #:match-font
   #:font-ascent #:font-descent #:font-line-gap))
