;;;; shape-demo.lisp — the shaping win: raw advances vs GPOS kerning + GSUB
;;;; ligatures (matched to HarfBuzz). sbcl --script demo/shape-demo.lisp
;;;;   -> /tmp/scribe-shape.png
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

(defun draw-raw (cv font text ppem x baseline color)
  "No shaping: cmap gid + raw hmtx advance, char by char."
  (let ((penx (float x 1d0)))
    (loop for ch across text do
      (let* ((gid (font-glyph-index font (char-code ch))) (sub (- penx (ffloor penx))))
        (multiple-value-bind (cov w h left top adv) (rasterize-glyph font gid ppem :subpixel sub)
          (when cov (blit cv cov w h (+ (floor penx) left) (+ baseline top) color))
          (incf penx adv))))))

(defun draw-shaped (cv font text ppem x baseline color)
  "Shaped: GSUB ligatures + GPOS kerning, with positioning offsets."
  (let* ((scale (/ (float ppem 1d0) (font-units-per-em font)))
         (buf (shape-run font text)) (penx (float x 1d0)))
    (loop for g across buf do
      (let* ((gx (+ penx (* (glyph-pos-x-offset g) scale))) (sub (- gx (ffloor gx))))
        (multiple-value-bind (cov w h left top adv) (rasterize-glyph font (glyph-pos-gid g) ppem :subpixel sub)
          (declare (ignore adv))
          (when cov (blit cv cov w h (+ (floor gx) left)
                          (- (+ baseline top) (round (* (glyph-pos-y-offset g) scale))) color)))
        (incf penx (* (glyph-pos-x-advance g) scale))))))

(let* ((here (directory-namestring *load-truename*))
       (sans (open-font (rd (merge-pathnames "../inspect/corpus/DejaVuSans.ttf" here))))
       (cv (make-canvas 940 470 '(250 250 250)))
       (ink '(20 20 24)) (muted '(150 150 158)))
  (draw-raw    cv sans "raw advances (no shaping):" 16 24 40 muted)
  (draw-raw    cv sans "AVATAR  Wave  To.  Yo.  Type" 52 24 100 ink)
  (draw-raw    cv sans "office  waffle  affluent  difficult" 40 24 158 ink)
  (draw-raw    cv sans "shaped — GPOS kerning + GSUB ligatures (= HarfBuzz):" 16 24 226 muted)
  (draw-shaped cv sans "AVATAR  Wave  To.  Yo.  Type" 52 24 286 ink)
  (draw-shaped cv sans "office  waffle  affluent  difficult" 40 24 344 ink)
  (draw-shaped cv sans "13px shaped body: the affluent office finds it difficult to fix waffles." 13 24 388 ink)
  (draw-raw    cv sans "13px raw body:    the affluent office finds it difficult to fix waffles." 13 24 410 muted)
  (write-png cv "/tmp/scribe-shape.png")
  (format t "~&wrote /tmp/scribe-shape.png~%"))
