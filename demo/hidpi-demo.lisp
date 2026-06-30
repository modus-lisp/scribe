;;;; hidpi-demo.lisp — the same body paragraph at 1x vs 2x (Retina) density,
;;;; grayscale + stem darkening, no hinting, no subpixel. The 2x block is what a
;;;; HiDPI display actually rasterizes for the same logical size.
;;;;   sbcl --script demo/hidpi-demo.lisp  -> /tmp/scribe-hidpi.png
(require :asdf)
(let ((here (directory-namestring *load-truename*)))
  (push (truename (merge-pathnames "../" here)) asdf:*central-registry*)
  (asdf:load-system "scribe"))
(in-package #:scribe)
(defun rd (p) (with-open-file (s p :element-type '(unsigned-byte 8))
                (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                  (read-sequence v s) v)))
(defun blit (cv cov w h ox oy color)
  (dotimes (yy h) (dotimes (xx w)
    (let ((c (aref cov (+ (* yy w) xx))))
      (when (> c 0d0) (blend-coverage cv (+ ox xx) (+ oy yy) c color))))))
(defun draw (cv font text ppem x baseline color)
  (let ((scale (/ (float ppem 1d0) (font-units-per-em font))) (penx (float x 1d0)))
    (loop for g across (shape-run font text) do
      (let* ((gx (+ penx (* (glyph-pos-x-offset g) scale))) (sub (- gx (ffloor gx))))
        (multiple-value-bind (cov w h left top) (rasterize-glyph font (glyph-pos-gid g) ppem :subpixel sub)
          (when cov (blit cv cov w h (+ (floor gx) left) (+ baseline top) color)))
        (incf penx (* (glyph-pos-x-advance g) scale))))))

(let* ((here (directory-namestring *load-truename*))
       (font (open-font (rd (merge-pathnames "../inspect/corpus/DejaVuSans.ttf" here))))
       (cv (make-canvas 820 360 '(252 252 252))) (ink '(26 26 30)) (muted '(140 140 148))
       (lines '("Typography is the craft of endowing human language with a"
                "durable visual form. The affluent office finds it difficult"
                "to fix the waffle — fi ffl ffi 0123456789.")))
  (let ((*stem-darkening* 0.7d0))
    (draw cv font "15px at 1x  (non-Retina):" 12 16 30 muted)
    (loop for l in lines for y from 56 by 22 do (draw cv font l 15 16 y ink))
    (draw cv font "30px at 2x  (Retina renders this for the same 15px logical):" 12 16 180 muted)
    (loop for l in lines for y from 214 by 40 do (draw cv font l 30 16 y ink)))
  (write-png cv "/tmp/scribe-hidpi.png")
  (format t "~&wrote /tmp/scribe-hidpi.png~%"))
