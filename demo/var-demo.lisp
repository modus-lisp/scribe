;;;; var-demo.lisp — variable fonts: New York swept across its design space
;;;; (wght 400..1000, opsz 12..256), fvar+avar+gvar validated against fontTools.
;;;;   sbcl --script demo/var-demo.lisp  -> /tmp/scribe-var.png
(require :asdf)
(let ((here (directory-namestring *load-truename*)))
  (push (truename (merge-pathnames "../" here)) asdf:*central-registry*)
  (asdf:load-system "scribe"))
(in-package #:scribe)
(defun rd (p) (with-open-file (s p :element-type '(unsigned-byte 8))
                (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                  (read-sequence v s) v)))
(defun blit (cv c w h ox oy col)
  (dotimes (yy h) (dotimes (xx w)
    (let ((v (aref c (+ (* yy w) xx))))
      (when (> v 0d0) (blend-coverage cv (+ ox xx) (+ oy yy) v col))))))
(defun draw (cv font text ppem loc x baseline col)
  (let ((norm (normalize-location font loc))
        (penx (float x 1d0)))
    (loop for ch across text do
      (let* ((gid (font-glyph-index font (char-code ch))) (sub (- penx (ffloor penx))))
        (multiple-value-bind (c w h left top adv) (rasterize-glyph font gid ppem :subpixel sub :variation norm)
          (when c (blit cv c w h (+ (floor penx) left) (+ baseline top) col))
          (incf penx adv))))))

(let* ((font (open-font (rd "/home/claude/scribe/inspect/corpus/NewYork.ttf")))
       (cv (make-canvas 900 560 '(252 252 252))) (ink '(20 20 26)) (g '(150 150 158))
       (s "Hamburgevons"))
  ;; weight sweep
  (draw cv font "weight  (wght 400 -> 1000)" 13 '(("opsz" . 256)) 16 28 g)
  (loop for w in '(400 568 674 810 1000) for y from 78 by 52
        do (draw cv font s 40 `(("opsz" . 256) ("wght" . ,w)) 16 y ink))
  ;; optical-size sweep (same ppem, design changes: display=high contrast, text=sturdy)
  (draw cv font "optical size  (opsz 256 -> 12, same pixel size)" 13 '(("opsz" . 256)) 16 360 g)
  (loop for o in '(256 60 24 12) for y from 410 by 48
        do (draw cv font s 40 `(("opsz" . ,o) ("wght" . 400)) 16 y ink))
  (write-png cv "/tmp/scribe-var.png")
  (format t "~&wrote /tmp/scribe-var.png~%"))
