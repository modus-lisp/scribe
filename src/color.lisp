;;;; color.lisp — COLR v0 / CPAL v0 colour glyphs (layered vector emoji).
;;;;
;;;; A COLR-v0 colour glyph is just a stack of ordinary outline glyphs, each
;;;; filled with a colour from the CPAL palette.  scribe already rasterizes
;;;; outlines and composites coverage, so colour emoji falls out of the existing
;;;; machinery: parse the two small tables, then for each layer rasterize the
;;;; layer glyph and blend it in its palette colour onto an RGBA canvas.  This is
;;;; the format Twemoji and many emoji fonts ship (vector, not bitmap), so it
;;;; scales cleanly.  (COLR v1 gradients and CBDT/sbix bitmaps are not handled.)

(in-package #:scribe)

;;; ---- CPAL v0: the colour palette --------------------------------------------

(defun %parse-cpal (font)
  "Palette 0 of CPAL as a simple-vector of (r g b) 8-bit lists; #() if absent."
  (multiple-value-bind (o len) (font-table font "CPAL")
    (declare (ignore len))
    (if (null o) #()
        (let* ((d (font-data font))
               (n-entries (u16 d (+ o 2)))
               (n-palettes (u16 d (+ o 4)))
               (records-off (+ o (u32 d (+ o 8))))
               (first (if (plusp n-palettes) (u16 d (+ o 12)) 0))   ; colorRecordIndices[0]
               (v (make-array n-entries)))
          (dotimes (i n-entries v)
            (let ((r (+ records-off (* 4 (+ first i)))))            ; record = B,G,R,A
              (setf (aref v i) (list (u8 d (+ r 2)) (u8 d (+ r 1)) (u8 d r)))))))))

;;; ---- COLR v0: base glyph -> layers ------------------------------------------

(defun %parse-colr (font)
  "Hash base-glyph-id -> vector of (layer-gid . palette-index), or NIL if absent."
  (multiple-value-bind (o len) (font-table font "COLR")
    (declare (ignore len))
    (when (and o (zerop (u16 (font-data font) o)))          ; version 0 only
      (let* ((d (font-data font))
             (n-base (u16 d (+ o 2)))
             (base-off (+ o (u32 d (+ o 4))))
             (layer-off (+ o (u32 d (+ o 8))))
             (h (make-hash-table)))
        (dotimes (i n-base h)
          (let* ((b (+ base-off (* 6 i)))
                 (gid (u16 d b)) (first (u16 d (+ b 2))) (nlayers (u16 d (+ b 4)))
                 (layers (make-array nlayers)))
            (dotimes (k nlayers)
              (let ((l (+ layer-off (* 4 (+ first k)))))
                (setf (aref layers k) (cons (u16 d l) (u16 d (+ l 2))))))
            (setf (gethash gid h) layers)))))))

;;; ---- per-font caches (COLR/CPAL are tiny; parse once) -----------------------

(defvar *cpal-cache* (make-hash-table :test 'eq))
(defvar *colr-cache* (make-hash-table :test 'eq))
(defun font-cpal (font) (or (gethash font *cpal-cache*) (setf (gethash font *cpal-cache*) (%parse-cpal font))))
(defun font-colr (font)
  (multiple-value-bind (v present) (gethash font *colr-cache*)
    (if present v (setf (gethash font *colr-cache*) (%parse-colr font)))))

(defun color-glyph-p (font gid)
  "True when GID is a COLR base glyph (has colour layers)."
  (let ((colr (font-colr font))) (and colr (nth-value 1 (gethash gid colr)))))

(defun color-glyph-layers (font gid)
  "List of (layer-gid r g b) for COLR base glyph GID, or NIL if it isn't colour."
  (let ((colr (font-colr font)))
    (when colr
      (let ((layers (gethash gid colr)) (cpal (font-cpal font)))
        (when layers
          (loop for pair across layers
                for col = (and (< (cdr pair) (length cpal)) (aref cpal (cdr pair)))
                collect (cons (car pair) (or col '(0 0 0)))))))))

;;; ---- rasterize a colour glyph -----------------------------------------------

(defun rasterize-color-glyph (font gid ppem &key (subpixel 0d0))
  "Composite the COLR layers of GID at PPEM into an RGBA bitmap.  Returns
   (values rgb alpha w h left top advance) — RGB is premultiplied sRGB, ALPHA the
   coverage plane, (LEFT,TOP) the origin relative to the pen (px, y-down) — or NIL
   when GID has no colour layers."
  (let ((layers (color-glyph-layers font gid)))
    (when layers
      (let ((rasters '()) (minx most-positive-fixnum) (miny most-positive-fixnum)
            (maxx most-negative-fixnum) (maxy most-negative-fixnum) (adv 0d0))
        (dolist (ly layers)
          (multiple-value-bind (cov w h left top a) (rasterize-glyph font (car ly) ppem :subpixel subpixel)
            (setf adv a)
            (when (and cov (plusp w) (plusp h))
              (push (list cov w h left top (cdr ly)) rasters)
              (setf minx (min minx left) miny (min miny top)
                    maxx (max maxx (+ left w)) maxy (max maxy (+ top h))))))
        (when rasters
          (let* ((cw (- maxx minx)) (chh (- maxy miny))
                 (cv (make-rgba-canvas cw chh)))
            (dolist (r (nreverse rasters))                     ; first layer at the bottom
              (destructuring-bind (cov w h left top color) r
                (let ((dx (- left minx)) (dy (- top miny)))
                  (dotimes (yy h)
                    (dotimes (xx w)
                      (let ((c (aref cov (+ (* yy w) xx))))
                        (when (> c 0d0) (blend-coverage cv (+ dx xx) (+ dy yy) c color))))))))
            (values (canvas-pixels cv) (canvas-alpha cv) cw chh minx miny adv)))))))
