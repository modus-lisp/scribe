;;;; text-demo.lisp — scribe's first real text: DejaVu glyphs rendered with the
;;;; analytic rasterizer, gamma-correct linear-light compositing, and subpixel
;;;; (fractional-pen) positioning.  Compare to weft's 7x13 bitmap.
;;;;   sbcl --script demo/text-demo.lisp   ->  /tmp/scribe-text.png
(require :asdf)
(let ((here (directory-namestring *load-truename*)))
  (push (truename (merge-pathnames "../" here)) asdf:*central-registry*)
  (asdf:load-system "scribe"))
(in-package #:scribe)

(defun rd (p) (with-open-file (s p :element-type '(unsigned-byte 8))
                (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                  (read-sequence v s) v)))

(defun draw-text (cv font text ppem x baseline color)
  "Lay TEXT along the baseline with fractional (subpixel) pen advances."
  (let ((penx (float x 1d0)))
    (loop for ch across text do
      (let* ((gid (font-glyph-index font (char-code ch)))
             (sub (- penx (ffloor penx))))
        (multiple-value-bind (cov w h left top adv) (rasterize-glyph font gid ppem :subpixel sub)
          (when cov
            (let ((ox (+ (floor penx) left)) (oy (+ baseline top)))
              (dotimes (yy h)
                (dotimes (xx w)
                  (let ((c (aref cov (+ (* yy w) xx))))
                    (when (> c 0d0) (blend-coverage cv (+ ox xx) (+ oy yy) c color)))))))
          (incf penx adv))))))

(let* ((here (directory-namestring *load-truename*))
       (sans (open-font (rd (merge-pathnames "../inspect/corpus/DejaVuSans.ttf" here))))
       (mono (open-font (rd (merge-pathnames "../inspect/corpus/DejaVuSansMono.ttf" here))))
       (cv (make-canvas 940 560 '(250 250 250)))
       (ink '(20 20 24)))
  (draw-text cv sans "Hamburgefonstiv 0123456789" 72 24 92 ink)
  (draw-text cv sans "Scribe: the quick brown fox jumps over the lazy dog." 36 24 160 ink)
  (draw-text cv sans "first-class text rendering in pure Common Lisp" 24 24 210 ink)
  (draw-text cv mono "(defun square (x) (* x x))   ; DejaVu Sans Mono, 20px" 20 24 250 ink)
  (draw-text cv sans "16px body — anti-aliased, gamma-correct, subpixel-positioned." 16 24 286 ink)
  (draw-text cv sans "13px — the size that separates homemade from first-class." 13 24 312 ink)
  ;; a size ramp of the same word to show even spacing / AA falloff
  (loop for ppem in '(11 13 16 20 26 34 44) and y = 360 then (+ y (+ ppem 8))
        do (draw-text cv sans (format nil "~dpx Scribe" ppem) ppem 24 (+ y ppem) ink))
  (write-png cv "/tmp/scribe-text.png")
  (format t "~&wrote /tmp/scribe-text.png~%"))
