;;;; glyf.lisp — TrueType glyph outlines (glyf/loca) + cmap dispatch.
;;;; Produces outlines as a list of
;;;; contours, each a list of segments in FONT UNITS:
;;;;   (:move x y) (:line x y) (:quad cx cy x y)
;;;; Simple + composite glyphs; the rasterizer flattens quads.
(in-package #:scribe)

(declaim (inline f2dot14))
(defun f2dot14 (d o) (/ (s16 d o) 16384d0))

(defun loca-offset (font gid)
  "Byte offset + length of glyph GID within the glyf table."
  (let* ((d (font-data font)) (lo (req-table font "loca"))
         (long (= (font-index-to-loc-format font) 1)))
    (if long
        (values (u32 d (+ lo (* gid 4))) (- (u32 d (+ lo (* (1+ gid) 4))) (u32 d (+ lo (* gid 4)))))
        (let ((a (* 2 (u16 d (+ lo (* gid 2))))) (b (* 2 (u16 d (+ lo (* (1+ gid) 2))))))
          (values a (- b a))))))

(defun %simple-glyph (d g ncont)
  "Parse a simple glyph at byte offset G with NCONT contours -> list of contours
   (each a list of (x y on-curve))."
  (let* ((endpts (make-array ncont))
         (p (+ g 10)))
    (dotimes (i ncont) (setf (aref endpts i) (u16 d p)) (incf p 2))
    (let* ((npts (if (zerop ncont) 0 (1+ (aref endpts (1- ncont)))))
           (ilen (u16 d p)) (flags (make-array npts)) (i 0))
      (incf p (+ 2 ilen))                                  ; skip instructions
      (loop while (< i npts) do                            ; flags (with repeat)
        (let ((f (u8 d p))) (incf p)
          (setf (aref flags i) f) (incf i)
          (when (logbitp 3 f)                              ; REPEAT
            (let ((r (u8 d p))) (incf p)
              (dotimes (_ r) (when (< i npts) (setf (aref flags i) f) (incf i)))))))
      (let ((xs (make-array npts)) (ys (make-array npts)) (x 0))
        (dotimes (k npts)                                  ; x deltas
          (let ((f (aref flags k)))
            (cond ((logbitp 1 f) (let ((dx (u8 d p))) (incf p) (incf x (if (logbitp 4 f) dx (- dx)))))
                  ((not (logbitp 4 f)) (incf x (s16 d p)) (incf p 2)))
            (setf (aref xs k) x)))
        (let ((y 0))
          (dotimes (k npts)                                ; y deltas
            (let ((f (aref flags k)))
              (cond ((logbitp 2 f) (let ((dy (u8 d p))) (incf p) (incf y (if (logbitp 5 f) dy (- dy)))))
                    ((not (logbitp 5 f)) (incf y (s16 d p)) (incf p 2)))
              (setf (aref ys k) y))))
        (values xs ys flags endpts)))))

(defun %points->contours (xs ys flags endpts ncont)
  "Build contour point-lists ((x y on-curve)*) from raw glyph points."
  (let ((contours '()) (start 0))
    (dotimes (c ncont (nreverse contours))
      (let ((end (aref endpts c)) (pts '()))
        (loop for k from start to end do
          (push (list (aref xs k) (aref ys k) (logbitp 0 (aref flags k))) pts))
        (push (nreverse pts) contours)
        (setf start (1+ end))))))

(defun %contour->segments (pts)
  "PTS = list of (x y on-curve) for one closed contour -> segment list
   ((:move ..) (:line ..)|(:quad ..)*). Implied on-curve midpoints inserted
   between consecutive off-curve points; contour explicitly closed."
  (let* ((v (coerce pts 'vector)) (n (length v)))
    (when (< n 2) (return-from %contour->segments nil))
    (let ((s (position-if #'third v)) (seq '()))
      (if s
          (dotimes (i n) (push (aref v (mod (+ s i) n)) seq))   ; rotate to on-curve start
          (progn                                                ; all off-curve: synth start
            (push (list (/ (+ (first (aref v 0)) (first (aref v (1- n)))) 2d0)
                        (/ (+ (second (aref v 0)) (second (aref v (1- n)))) 2d0) t) seq)
            (dotimes (i n) (push (aref v i) seq))))
      (setf seq (nreverse seq))
      (setf seq (append seq (list (first seq))))              ; close back to start
      (let* ((p0 (first seq))
             (sx (float (first p0) 1d0)) (sy (float (second p0) 1d0))
             (segs (list (list :move sx sy))) (cx nil) (cy nil))
        (dolist (q (rest seq))
          (let ((qx (float (first q) 1d0)) (qy (float (second q) 1d0)) (qon (third q)))
            (if qon
                (if cx (progn (push (list :quad cx cy qx qy) segs) (setf cx nil))
                    (push (list :line qx qy) segs))
                (if cx
                    (let ((mx (/ (+ cx qx) 2d0)) (my (/ (+ cy qy) 2d0)))
                      (push (list :quad cx cy mx my) segs) (setf cx qx cy qy))
                    (setf cx qx cy qy)))))
        (when cx (push (list :quad cx cy sx sy) segs))
        (nreverse segs)))))

(defun glyph-outline (font gid &key variation)
  "Return GID's outline as a list of contours (each a segment list) in font units.
   Dispatches to CFF (cubic) for OTTO fonts, else glyf/loca (quadratic)."
  (declare (ignore variation))
  (when (font-table font "CFF ")
    (return-from glyph-outline (cff-glyph-outline font gid)))
  (multiple-value-bind (off len) (loca-offset font gid)
    (when (zerop len) (return-from glyph-outline nil))     ; empty glyph (e.g. space)
    (let* ((d (font-data font)) (g (+ (req-table font "glyf") off))
           (ncont (s16 d g)))
      (if (>= ncont 0)
          (multiple-value-bind (xs ys flags endpts) (%simple-glyph d g ncont)
            (when variation                                  ; apply gvar deltas
              (apply-gvar font gid xs ys variation))
            (mapcan (lambda (c) (let ((s (%contour->segments c))) (and s (list s))))
                    (%points->contours xs ys flags endpts ncont)))
          (%composite-glyph font d g variation)))))

(defun %composite-glyph (font d g &optional variation)
  "Parse a composite glyph -> merged contours (recursive, with 2x2+offset xform).
   VARIATION is threaded to components so their gvar deltas apply."
  (let ((p (+ g 10)) (out '()) (more t))
    (loop while more do
      (let* ((flags (u16 d p)) (cgid (u16 d (+ p 2))) (dx 0) (dy 0)
             (a 1d0) (b 0d0) (c 0d0) (e 1d0))
        (incf p 4)
        (if (logbitp 0 flags)                              ; ARG_1_AND_2_ARE_WORDS
            (progn (setf dx (s16 d p) dy (s16 d (+ p 2))) (incf p 4))
            (progn (setf dx (let ((v (u8 d p))) (if (>= v 128) (- v 256) v))
                         dy (let ((v (u8 d (1+ p)))) (if (>= v 128) (- v 256) v))) (incf p 2)))
        (cond ((logbitp 3 flags) (setf a (f2dot14 d p) e a) (incf p 2))            ; WE_HAVE_A_SCALE
              ((logbitp 6 flags) (setf a (f2dot14 d p) e (f2dot14 d (+ p 2))) (incf p 4)) ; X_AND_Y_SCALE
              ((logbitp 7 flags) (setf a (f2dot14 d p) b (f2dot14 d (+ p 2))      ; TWO_BY_TWO
                                       c (f2dot14 d (+ p 4)) e (f2dot14 d (+ p 6))) (incf p 8)))
        (dolist (contour (glyph-outline font cgid :variation variation)) ; recurse + transform
          (push (mapcar (lambda (seg)
                          (cons (car seg)
                                (loop for (x y) on (cdr seg) by #'cddr
                                      collect (+ (* a x) (* c y) dx)
                                      collect (+ (* b x) (* e y) dy))))
                        contour)
                out))
        (setf more (logbitp 5 flags))))                    ; MORE_COMPONENTS
    (nreverse out)))

;;; ---- cmap dispatch: pick the best Unicode subtable, map codepoint -> gid ----
(defun font-glyph-index (font codepoint)
  "Map a Unicode CODEPOINT to a glyph id via the best available cmap subtable."
  (let ((subs (cmap-subtables font)) (d (font-data font)))
    (flet ((find-sub (pid eid fmt) (find-if (lambda (s) (and (= (first s) pid) (= (second s) eid)
                                                             (= (third s) fmt))) subs)))
      (let* ((best (or (find-sub 3 10 12) (find-sub 0 10 12)   ; full Unicode
                       (find-sub 3 1 4)   (find-sub 0 3 4)     ; BMP
                       (find-sub 0 4 12)  (find-sub 0 6 12)
                       (first subs)))
             (ht (when best
                   (ecase (third best)
                     (4  (parse-cmap-4  d (fourth best)))
                     (6  (parse-cmap-6  d (fourth best)))
                     (12 (parse-cmap-12 d (fourth best)))))))
        (or (and ht (gethash codepoint ht)) 0)))))
