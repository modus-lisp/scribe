;;;; raster.lisp — analytic-coverage outline rasterizer (signed-area accumulation,
;;;; the font-rs / FreeType-smooth lineage; NOT supersampling).
;;;;
;;;; Outline (font units) -> scale to ppem -> flatten quadratics to line segments
;;;; -> accumulate exact signed area into a w*h buffer -> running prefix sum per
;;;; row gives per-pixel coverage in [0,1]. Closed contours make each row's deltas
;;;; sum to zero, so the running accumulation self-resets at row boundaries.
(in-package #:scribe)

(defstruct (raster (:constructor %make-raster))
  (w 0 :type fixnum) (h 0 :type fixnum)
  (a nil :type (simple-array double-float (*))))   ; signed-area accumulation

(defun %acc-line (r x0 y0 x1 y1)
  "Accumulate one device-space line segment into R's area buffer (font-rs kernel)."
  (declare (type raster r) (type double-float x0 y0 x1 y1))
  (when (= y0 y1) (return-from %acc-line))
  (let ((w (raster-w r)) (h (raster-h r)) (a (raster-a r)) (dir 1d0))
    (declare (type (simple-array double-float (*)) a))
    (when (> y0 y1) (rotatef x0 x1) (rotatef y0 y1) (setf dir -1d0))
    (let* ((dxdy (/ (- x1 x0) (- y1 y0)))
           (x x0)
           (yclip0 (max y0 0d0)) (yclip1 (min y1 (float h 1d0))))
      (when (< y0 0d0) (setf x (+ x0 (* (- 0d0 y0) dxdy))))
      (loop for y from (floor yclip0) below (ceiling yclip1)
            while (< y h) do
        (let* ((linestart (* y w))
               (dy (- (min (float (1+ y) 1d0) yclip1) (max (float y 1d0) yclip0))))
          (when (plusp dy)
            (let* ((xnext (+ x (* dxdy dy)))
                   (d (* dy dir))
                   (xa (min x xnext)) (xb (max x xnext))
                   (xaf (floor xa)) (xbi (ceiling xb)))
              (cond
                ((<= xbi (+ xaf 1))                       ; within one pixel column
                 (when (and (>= xaf 0) (< xaf w))
                   (let ((xmf (- (* 0.5d0 (+ x xnext)) xaf)))
                     (incf (aref a (+ linestart xaf)) (* d (- 1d0 xmf)))
                     (when (< (+ xaf 1) w) (incf (aref a (+ linestart xaf 1)) (* d xmf))))))
                (t                                        ; spans multiple columns
                 (let* ((s (/ 1d0 (- xb xa)))
                        (x0f (- xa xaf))
                        (oneminus (- 1d0 x0f))
                        (a0 (* 0.5d0 s oneminus oneminus))
                        (x1f (+ (- xb xbi) 1d0))
                        (am (* 0.5d0 s x1f x1f)))
                   (when (and (>= xaf 0) (< xaf w)) (incf (aref a (+ linestart xaf)) (* d a0)))
                   (if (= xbi (+ xaf 2))
                       (let ((i (+ xaf 1))) (when (and (>= i 0) (< i w))
                                              (incf (aref a (+ linestart i)) (* d (- 1d0 a0 am)))))
                       (let ((a1 (* s (- 1.5d0 x0f))))
                         (let ((i (+ xaf 1))) (when (and (>= i 0) (< i w))
                                                (incf (aref a (+ linestart i)) (* d (- a1 a0)))))
                         (loop for xi from (+ xaf 2) below (1- xbi)
                               when (and (>= xi 0) (< xi w))
                                 do (incf (aref a (+ linestart xi)) (* d s)))
                         (let ((a2 (+ a1 (* (float (- xbi xaf 3) 1d0) s)))
                               (i (1- xbi)))
                           (when (and (>= i 0) (< i w))
                             (incf (aref a (+ linestart i)) (* d (- 1d0 a2 am)))))))
                   (when (and (>= xbi 0) (< xbi w)) (incf (aref a (+ linestart xbi)) (* d am))))))
              (setf x xnext))))))))

(defparameter *flatten-tol* 0.08d0 "Max chord deviation (device px) when flattening curves.")

(declaim (inline %dist-to-chord))
(defun %dist-to-chord (px py x0 y0 x1 y1)
  "Perpendicular distance of (px,py) from the chord (x0,y0)-(x1,y1)."
  (declare (type double-float px py x0 y0 x1 y1))
  (let ((dx (- x1 x0)) (dy (- y1 y0)))
    (let ((l2 (+ (* dx dx) (* dy dy))))
      (if (< l2 1d-9)
          (+ (abs (- px x0)) (abs (- py y0)))
          (/ (abs (- (* (- px x0) dy) (* (- py y0) dx))) (sqrt l2))))))

(defun %flatten-quad (r x0 y0 cx cy x1 y1 &optional (depth 0))
  "Recursively subdivide a quadratic Bezier until flat to *flatten-tol*."
  (declare (type double-float x0 y0 cx cy x1 y1))
  (if (or (>= depth 20) (<= (%dist-to-chord cx cy x0 y0 x1 y1) *flatten-tol*))
      (%acc-line r x0 y0 x1 y1)
      (let* ((ax (* 0.5d0 (+ x0 cx))) (ay (* 0.5d0 (+ y0 cy)))
             (bx (* 0.5d0 (+ cx x1))) (by (* 0.5d0 (+ cy y1)))
             (mx (* 0.5d0 (+ ax bx))) (my (* 0.5d0 (+ ay by))))
        (%flatten-quad r x0 y0 ax ay mx my (1+ depth))
        (%flatten-quad r mx my bx by x1 y1 (1+ depth)))))

;;; tiny pen state for rasterize-outline (move/line/quad chaining)
(defvar *lpx* 0d0) (defvar *lpy* 0d0)
(defun %last-point () (cons *lpx* *lpy*))
(defun %set-last-point (seg tx ty)
  (let ((tail (last (cdr seg) 2)))
    (setf *lpx* (funcall tx (first tail)) *lpy* (funcall ty (second tail)))))

(defun %flatten-cubic (r x0 y0 c1x c1y c2x c2y x1 y1 &optional (depth 0))
  "Recursively subdivide a cubic Bezier until both controls are flat to tol."
  (declare (type double-float x0 y0 c1x c1y c2x c2y x1 y1))
  (if (or (>= depth 20)
          (and (<= (%dist-to-chord c1x c1y x0 y0 x1 y1) *flatten-tol*)
               (<= (%dist-to-chord c2x c2y x0 y0 x1 y1) *flatten-tol*)))
      (%acc-line r x0 y0 x1 y1)
      (let* ((p01x (* 0.5d0 (+ x0 c1x))) (p01y (* 0.5d0 (+ y0 c1y)))
             (p12x (* 0.5d0 (+ c1x c2x))) (p12y (* 0.5d0 (+ c1y c2y)))
             (p23x (* 0.5d0 (+ c2x x1))) (p23y (* 0.5d0 (+ c2y y1)))
             (ax (* 0.5d0 (+ p01x p12x))) (ay (* 0.5d0 (+ p01y p12y)))
             (bx (* 0.5d0 (+ p12x p23x))) (by (* 0.5d0 (+ p12y p23y)))
             (mx (* 0.5d0 (+ ax bx))) (my (* 0.5d0 (+ ay by))))
        (%flatten-cubic r x0 y0 p01x p01y ax ay mx my (1+ depth))
        (%flatten-cubic r mx my bx by p23x p23y x1 y1 (1+ depth)))))

(defun rasterize-outline (contours scale &key (dx 0d0) (dy 0d0) origin-x origin-y)
  "Scan-convert CONTOURS (font units) at SCALE px/unit with fractional pen offset
   (DX,DY) into a coverage bitmap. ORIGIN-X/Y (font units) map to bitmap (0,0);
   y is flipped (font y-up -> bitmap y-down). Returns (values coverage w h)."
  (let* ((minx 1d30) (miny 1d30) (maxx -1d30) (maxy -1d30))
    ;; device-space bbox to size the bitmap
    (dolist (c contours)
      (dolist (seg c)
        (loop for (x y) on (cdr seg) by #'cddr do
          (setf minx (min minx x) maxx (max maxx x) miny (min miny y) maxy (max maxy y)))))
    (when (> minx maxx) (return-from rasterize-outline (values nil 0 0)))
    (let* ((ox (or origin-x minx)) (oy (or origin-y maxy))
           (w (+ 2 (ceiling (* (- maxx ox) scale))))
           (h (+ 2 (ceiling (* (- oy miny) scale))))
           (w (max 1 w)) (h (max 1 h))
           (r (%make-raster :w w :h h :a (make-array (* w h) :element-type 'double-float
                                                              :initial-element 0d0))))
      (flet ((tx (x) (+ (* (- x ox) scale) dx))
             (ty (y) (+ (* (- oy y) scale) dy)))
        (dolist (c contours)
          (let ((mvx 0d0) (mvy 0d0))                ; contour start, for explicit close
            (dolist (seg c)
              (ecase (car seg)
                (:move (setf mvx (tx (second seg)) mvy (ty (third seg))))
                (:line (destructuring-bind (x y) (cdr seg)
                         (let ((p (%last-point))) (%acc-line r (car p) (cdr p) (tx x) (ty y)))))
                (:quad (destructuring-bind (cx cy x y) (cdr seg)
                         (let ((p (%last-point)))
                           (%flatten-quad r (car p) (cdr p) (tx cx) (ty cy) (tx x) (ty y)))))
                (:cubic (destructuring-bind (c1x c1y c2x c2y x y) (cdr seg)
                          (let ((p (%last-point)))
                            (%flatten-cubic r (car p) (cdr p) (tx c1x) (ty c1y)
                                            (tx c2x) (ty c2y) (tx x) (ty y))))))
              (%set-last-point seg #'tx #'ty))
            (%acc-line r *lpx* *lpy* mvx mvy)))      ; close contour (no-op if already closed)
        ;; prefix-sum each row -> coverage
        (let ((cov (make-array (* w h) :element-type 'double-float)))
          (dotimes (y h)
            (let ((acc 0d0) (row (* y w)))
              (dotimes (x w)
                (incf acc (aref (raster-a r) (+ row x)))
                (setf (aref cov (+ row x)) (min 1d0 (abs acc))))))
          (values cov w h))))))

(defun rasterize-glyph (font gid ppem &key (subpixel 0d0) variation)
  "Rasterize GID at PPEM px/em. Returns (values coverage w h left top advance)
   where (left,top) is the bitmap origin relative to the pen (px, y-down) and
   ADVANCE is the horizontal advance in px. SUBPIXEL in [0,1) shifts x."
  (let* ((upem (font-units-per-em font))
         (scale (/ (float ppem 1d0) upem))
         (outline (glyph-outline font gid :variation variation)))
    (let ((adv (advance-at font gid variation)))   ; hmtx + HVAR/gvar advance delta
      (if (null outline)
          (values nil 0 0 0 0 (* adv scale))
          ;; bbox in font units
          (let ((minx 1d30) (miny 1d30) (maxx -1d30) (maxy -1d30))
            (dolist (c outline)
              (dolist (seg c)
                (loop for (x y) on (cdr seg) by #'cddr do
                  (setf minx (min minx x) maxx (max maxx x) miny (min miny y) maxy (max maxy y)))))
            (multiple-value-bind (cov w h)
                (rasterize-outline outline scale :dx subpixel :origin-x minx :origin-y maxy)
              (values cov w h
                      (floor (* minx scale))         ; left bearing in px
                      (- (ceiling (* maxy scale)))   ; top above baseline (y-down)
                      (* adv scale))))))))

(defun glyph-advance (font gid)
  "Horizontal advance + lsb (font units) from hmtx."
  (let* ((d (font-data font)) (o (req-table font "hmtx"))
         (nh (font-num-h-metrics font)))
    (if (< gid nh)
        (values (u16 d (+ o (* gid 4))) (s16 d (+ o (* gid 4) 2)))
        (values (u16 d (+ o (* (1- nh) 4)))
                (s16 d (+ o (* nh 4) (* (- gid nh) 2)))))))
