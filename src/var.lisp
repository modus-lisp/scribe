;;;; var.lisp — OpenType variable fonts: fvar axes, avar remap, gvar deltas.
;;;; STRONG-TIER (coupled to the outline model). Applies glyph variation deltas
;;;; for a design-space LOCATION (alist of axis-tag -> user value) before the
;;;; outline is built, plus advance deltas from gvar phantom points.
(in-package #:scribe)

(declaim (inline fixed16.16))
(defun fixed16.16 (d o)
  (let ((v (u32 d o))) (/ (if (>= v #x80000000) (- v #x100000000) v) 65536d0)))

;;; ---- fvar: axes ----
(defstruct fvar-axis tag min default max)

(defun font-fvar (font)
  "List of fvar-axis in table order (cached)."
  (or (font-%fvar font)
      (setf (font-%fvar font)
            (let ((o (font-table font "fvar")))
              (when o
                (let* ((d (font-data font))
                       (axes-off (+ o (u16 d (+ o 4))))
                       (n (u16 d (+ o 8))) (sz (u16 d (+ o 10))) (out '()))
                  (dotimes (i n (nreverse out))
                    (let ((a (+ axes-off (* i sz))))
                      (push (make-fvar-axis :tag (tag d a)
                                            :min (fixed16.16 d (+ a 4))
                                            :default (fixed16.16 d (+ a 8))
                                            :max (fixed16.16 d (+ a 12))) out)))))))))

;;; ---- avar: piecewise-linear remap of normalized coords ----
(defun font-avar (font)
  "Vector of segment maps (one per axis); each a list of (from . to) F2Dot14 pairs."
  (or (font-%avar font)
      (setf (font-%avar font)
            (let ((o (font-table font "avar")))
              (if (null o) :none
                  (let* ((d (font-data font))
                         (axis-count (u16 d (+ o 6)))
                         (p (+ o 8)) (maps (make-array axis-count)))
                    (dotimes (ax axis-count maps)
                      (let ((cnt (u16 d p)) (pairs '()))
                        (incf p 2)
                        (dotimes (i cnt)
                          (push (cons (/ (s16 d p) 16384d0) (/ (s16 d (+ p 2)) 16384d0)) pairs)
                          (incf p 4))
                        (setf (aref maps ax) (nreverse pairs))))))))))

(defun %avar-remap (segs n)
  "Map normalized coord N through the (from . to) segment list SEGS."
  (when (or (null segs) (< (length segs) 2)) (return-from %avar-remap n))
  (loop for (a b) on segs while b
        for fa = (car a) for ta = (cdr a) for fb = (car b) for tb = (cdr b)
        when (and (>= n fa) (<= n fb))
          do (return-from %avar-remap
               (if (= fb fa) ta (+ ta (* (- tb ta) (/ (- n fa) (- fb fa))))))
        finally (return n)))

;;; ---- normalize a user location to post-avar coords (vector, axis order) ----
(defun normalize-location (font location)
  "LOCATION = alist (axis-tag-string . user-value). Returns a double-float vector
   of normalized, avar-remapped coordinates in fvar axis order."
  (let* ((axes (font-fvar font)) (avar (font-avar font))
         (out (make-array (length axes) :element-type 'double-float :initial-element 0d0)))
    (loop for axis in axes for i from 0
          for uv = (cdr (assoc (fvar-axis-tag axis) location :test #'string=))
          for v = (if uv (float uv 1d0) (fvar-axis-default axis))
          for n = (let ((v (max (fvar-axis-min axis) (min (fvar-axis-max axis) v)))
                        (def (fvar-axis-default axis)))
                    (cond ((< v def) (if (= def (fvar-axis-min axis)) 0d0
                                         (/ (- v def) (- def (fvar-axis-min axis)))))
                          ((> v def) (if (= (fvar-axis-max axis) def) 0d0
                                         (/ (- v def) (- (fvar-axis-max axis) def))))
                          (t 0d0)))
          do (setf (aref out i)
                   (max -1d0 (min 1d0 (if (and (vectorp avar) (< i (length avar)))
                                          (%avar-remap (aref avar i) n) n)))))
    out))

;;; ---- gvar: packed point numbers + packed deltas ----
(defun %read-packed-points (d p)
  "Return (values points-vector-or-:all new-p). :all = deltas for every point."
  (let ((b (u8 d p)))
    (if (< b #x80)
        (if (zerop b) (values :all (1+ p))
            (%read-point-runs d (1+ p) b))
        (%read-point-runs d (+ p 2) (logior (ash (logand b #x7f) 8) (u8 d (1+ p)))))))

(defun %read-point-runs (d p count)
  (let ((pts (make-array count)) (got 0) (prev 0))
    (loop while (< got count) do
      (let* ((ctrl (u8 d p)) (run (1+ (logand ctrl #x7f))) (word (logbitp 7 ctrl)))
        (incf p)
        (dotimes (_ run)
          (when (< got count)
            (if word (progn (incf prev (u16 d p)) (incf p 2))
                (progn (incf prev (u8 d p)) (incf p)))
            (setf (aref pts got) prev) (incf got)))))
    (values pts p)))

(defun %read-packed-deltas (d p n)
  "Read N packed deltas -> (values delta-vector new-p)."
  (let ((out (make-array n :initial-element 0)) (got 0))
    (loop while (< got n) do
      (let* ((ctrl (u8 d p)) (run (1+ (logand ctrl #x3f))))
        (incf p)
        (cond ((logbitp 7 ctrl) (dotimes (_ run) (when (< got n) (setf (aref out got) 0) (incf got)))) ; zeros
              ((logbitp 6 ctrl) (dotimes (_ run) (when (< got n) (setf (aref out got) (s16 d p)) (incf p 2) (incf got))))
              (t (dotimes (_ run) (when (< got n)
                                    (setf (aref out got) (let ((v (u8 d p))) (if (>= v 128) (- v 256) v)))
                                    (incf p) (incf got)))))))
    (values out p)))

(defun %f2dot14-vec (d p n)
  (let ((v (make-array n :element-type 'double-float)))
    (dotimes (i n v) (setf (aref v i) (/ (s16 d (+ p (* i 2))) 16384d0)))))

(defun %glyph-point-count (font gid)
  "Number of contour points of simple glyph GID, or NIL if composite/empty."
  (multiple-value-bind (off len) (loca-offset font gid)
    (when (zerop len) (return-from %glyph-point-count 0))
    (let* ((d (font-data font)) (g (+ (req-table font "glyf") off)) (ncont (s16 d g)))
      (when (>= ncont 0)
        (if (zerop ncont) 0 (1+ (u16 d (+ g 10 (* 2 (1- ncont))))))))))

(defun gvar-summed-deltas (font gid norm)
  "Return (values xdense ydense npts) — summed gvar deltas over all tuples for
   simple glyph GID at normalized location NORM. NIL if no variation/composite."
  (let ((o (font-table font "gvar")) (npts (%glyph-point-count font gid)))
    (when (or (null o) (null npts) (zerop npts)) (return-from gvar-summed-deltas nil))
    (let* ((d (font-data font))
           (axis-count (u16 d (+ o 4)))
           (shared-off (+ o (u32 d (+ o 8))))
           (long (logbitp 0 (u16 d (+ o 14)))) (arr-off (+ o (u32 d (+ o 16))))
           (total (+ npts 4))
           (og (if long (u32 d (+ o 20 (* gid 4))) (* 2 (u16 d (+ o 20 (* gid 2))))))
           (on (if long (u32 d (+ o 20 (* (1+ gid) 4))) (* 2 (u16 d (+ o 20 (* (1+ gid) 2)))))))
      (when (= og on) (return-from gvar-summed-deltas nil))
      (let* ((gv (+ arr-off og))
             (tvc (u16 d gv)) (count (logand tvc #x0fff)) (shared-pts-flag (logbitp 15 tvc))
             (sp (+ gv (u16 d (+ gv 2))))                    ; serialized-data pointer
             (p (+ gv 4))                                    ; tuple-header pointer
             (xd (make-array total :element-type 'double-float :initial-element 0d0))
             (yd (make-array total :element-type 'double-float :initial-element 0d0))
             (shared-pts :all))
        (when shared-pts-flag
          (multiple-value-bind (pts np) (%read-packed-points d sp) (setf shared-pts pts sp np)))
        (dotimes (_ count)
          (let* ((vsize (u16 d p)) (tidx (u16 d (+ p 2))) (peak nil) (lower nil) (upper nil))
            (incf p 4)
            (if (logbitp 15 tidx)
                (progn (setf peak (%f2dot14-vec d p axis-count)) (incf p (* 2 axis-count)))
                (setf peak (%f2dot14-vec d (+ shared-off (* (logand tidx #x0fff) axis-count 2)) axis-count)))
            (if (logbitp 14 tidx)
                (progn (setf lower (%f2dot14-vec d p axis-count)
                             upper (%f2dot14-vec d (+ p (* 2 axis-count)) axis-count))
                       (incf p (* 4 axis-count)))
                (progn (setf lower (make-array axis-count :element-type 'double-float)
                             upper (make-array axis-count :element-type 'double-float))
                       (dotimes (a axis-count)
                         (setf (aref lower a) (min 0d0 (aref peak a))
                               (aref upper a) (max 0d0 (aref peak a))))))
            (let ((scalar (gvar-scalar peak lower upper norm)))
              (when (/= scalar 0d0)
                (let ((tsp sp) (pts shared-pts))
                  (when (logbitp 13 tidx)                    ; private point numbers
                    (multiple-value-bind (pp np) (%read-packed-points d tsp) (setf pts pp tsp np)))
                  (let ((nset (if (eq pts :all) total (length pts))))
                    (multiple-value-bind (dx np) (%read-packed-deltas d tsp nset)
                      (multiple-value-bind (dy np2) (%read-packed-deltas d np nset)
                        (declare (ignore np2))
                        (if (eq pts :all)
                            (dotimes (k total) (incf (aref xd k) (* scalar (aref dx k)))
                                                (incf (aref yd k) (* scalar (aref dy k))))
                            (dotimes (j nset)
                              (let ((k (aref pts j)))
                                (when (< k total)
                                  (incf (aref xd k) (* scalar (aref dx j)))
                                  (incf (aref yd k) (* scalar (aref dy j))))))))))))
              (setf sp (+ sp vsize)))))
        (values xd yd npts)))))

(defun gvar-scalar (peak lower upper norm)
  "Tuple interpolation scalar (matches fontTools supportScalar)."
  (let ((s 1d0))
    (dotimes (i (length peak) s)
      (let ((pk (aref peak i)) (v (aref norm i)))
        (unless (= pk 0d0)
          (let ((lo (aref lower i)) (up (aref upper i)))
            (cond ((= v pk))                                  ; *1
                  ((or (<= v lo) (>= v up)) (return 0d0))
                  ((< v pk) (setf s (* s (/ (- v lo) (- pk lo)))))
                  (t        (setf s (* s (/ (- up v) (- up pk))))))))))))

(defun apply-gvar (font gid xs ys norm)
  "Mutate contour points XS/YS by the summed gvar deltas at NORM (in place)."
  (multiple-value-bind (xd yd npts) (gvar-summed-deltas font gid norm)
    (when xd
      (dotimes (k npts)
        (setf (aref xs k) (+ (aref xs k) (aref xd k))
              (aref ys k) (+ (aref ys k) (aref yd k)))))))

(defun varied-advance (font gid base norm)
  "Base advance (font units) + gvar phantom-point advance delta at NORM."
  (if (null norm) base
      (multiple-value-bind (xd yd npts) (gvar-summed-deltas font gid norm)
        (declare (ignore yd))
        (if xd (+ base (- (aref xd (1+ npts)) (aref xd npts))) base))))
