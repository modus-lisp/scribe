;;;; cff-demo.lisp — CFF/Type2 outlines (URW serif + sans) through the same
;;;; analytic, gamma-correct, subpixel pipeline as glyf.
;;;;   sbcl --script demo/cff-demo.lisp   ->  /tmp/scribe-cff.png
(require :asdf)
(let ((here (directory-namestring *load-truename*)))
  (push (truename (merge-pathnames "../" here)) asdf:*central-registry*)
  (asdf:load-system "scribe"))
(in-package #:scribe)

(defun rd (p) (with-open-file (s p :element-type '(unsigned-byte 8))
                (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                  (read-sequence v s) v)))
(defun draw-text (cv font text ppem x baseline color)
  (let ((penx (float x 1d0)))
    (loop for ch across text do
      (let* ((gid (font-glyph-index font (char-code ch))) (sub (- penx (ffloor penx))))
        (multiple-value-bind (cov w h left top adv) (rasterize-glyph font gid ppem :subpixel sub)
          (when cov
            (let ((ox (+ (floor penx) left)) (oy (+ baseline top)))
              (dotimes (yy h) (dotimes (xx w)
                (let ((c (aref cov (+ (* yy w) xx))))
                  (when (> c 0d0) (blend-coverage cv (+ ox xx) (+ oy yy) c color)))))))
          (incf penx adv))))))

(let* ((here (directory-namestring *load-truename*))
       (corp (lambda (n) (open-font (rd (merge-pathnames (format nil "../inspect/corpus/~a" n) here)))))
       (serif (funcall corp "C059-Roman.otf"))
       (sans  (funcall corp "NimbusSans-Regular.otf"))
       (cv (make-canvas 940 470 '(250 250 250)))
       (ink '(20 20 24)))
  (draw-text cv serif "Typography (CFF serif) — Quartz jocks vex." 56 24 78 ink)
  (draw-text cv serif "C059 Roman, a Century clone: fi ff handgloves 0123456789" 30 24 138 ink)
  (draw-text cv sans  "Nimbus Sans (CFF) — the quick brown fox jumps." 40 24 200 ink)
  (draw-text cv serif "16px serif body: cubic Beziers, flex hints, analytic AA, linear blend." 16 24 244 ink)
  (draw-text cv serif "13px — Hamburgefonstiv — the proof is in the small sizes." 13 24 270 ink)
  (loop for ppem in '(12 15 19 24 30 38) and y = 312 then (+ y (+ ppem 9))
        do (draw-text cv serif (format nil "~dpx  Scribe renders CFF" ppem) ppem 24 (+ y ppem) ink))
  (write-png cv "/tmp/scribe-cff.png")
  (format t "~&wrote /tmp/scribe-cff.png~%"))
