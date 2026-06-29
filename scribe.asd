;;;; scribe.asd — first-class text rendering in pure Common Lisp.
(asdf:defsystem :scribe
  :description "First-class text rendering in pure Common Lisp: sfnt/OpenType
parsing, OpenType-Layout shaping, analytic-coverage rasterization, and
gamma-correct linear-light compositing. No FFI, no FreeType, no HarfBuzz."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "blend")     ; DONE: gamma-correct linear-light compositing + canvas + PNG
     (:file "font")      ; scaffold: sfnt/OpenType parsing
     (:file "raster")    ; scaffold: analytic-coverage rasterizer
     (:file "shape")))))  ; scaffold: GSUB/GPOS shaping
