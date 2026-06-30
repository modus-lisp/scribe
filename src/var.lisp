;;;; var.lisp — OpenType variable fonts: fvar axes, avar remap, gvar deltas.
;;;; Applies glyph variation deltas for a design-space LOCATION
;;;; (alist of axis-tag -> user value) before the
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

;;; ---- HVAR: horizontal-metrics variations (advance-width deltas) ----
;;; The canonical advance-variation source (preferred over gvar phantom points
;;; when present). Item Variation Store + advance-width DeltaSetIndexMap.

(defun %region-scalar (d region-off axis-count norm)
  "Scalar for one variation region (axisCount RegionAxisCoordinates) at NORM."
  (let ((s 1d0))
    (dotimes (a axis-count s)
      (let* ((r (+ region-off (* a 6)))
             (start (/ (s16 d r) 16384d0)) (peak (/ (s16 d (+ r 2)) 16384d0))
             (end (/ (s16 d (+ r 4)) 16384d0)) (v (aref norm a)))
        (unless (= peak 0d0)
          (cond ((or (< v start) (> v end)) (return 0d0))
                ((= v peak))
                ((< v peak) (setf s (* s (/ (- v start) (- peak start)))))
                (t (setf s (* s (/ (- end v) (- end peak)))))))))))

(defun %ivs-delta (d ivs outer inner norm)
  "Evaluate Item Variation Store delta for (outer,inner) at NORM."
  (let* ((rlist (+ ivs (u32 d (+ ivs 2))))
         (axis-count (u16 d rlist))
         (ivd (+ ivs (u32 d (+ ivs 8 (* outer 4)))))
         (wc-raw (u16 d (+ ivd 2)))
         (long (logbitp 15 wc-raw)) (wc (logand wc-raw #x7fff))
         (ric (u16 d (+ ivd 4)))
         (ridx-off (+ ivd 6))
         (row-size (+ (* wc (if long 4 2)) (* (- ric wc) (if long 2 1))))
         (row (+ ridx-off (* ric 2) (* inner row-size)))
         (delta 0d0))
    (dotimes (j ric (round delta))
      (let* ((ridx (u16 d (+ ridx-off (* j 2))))
             (scalar (%region-scalar d (+ rlist 4 (* ridx axis-count 6)) axis-count norm))
             (dval (if (< j wc)
                       (if long (let ((v (u32 d (+ row (* j 4))))) (if (>= v #x80000000) (- v #x100000000) v))
                           (s16 d (+ row (* j 2))))
                       (let ((b (+ row (* wc (if long 4 2)))) (k (- j wc)))
                         (if long (s16 d (+ b (* k 2)))
                             (let ((v (u8 d (+ b k)))) (if (>= v 128) (- v 256) v)))))))
        (incf delta (* scalar dval))))))

(defun %delta-set-index (d o gid)
  "DeltaSetIndexMap lookup -> (values outer inner) for GID."
  (let ((fmt (u8 d o)) (ef (u8 d (+ o 1))))
    (multiple-value-bind (mapcount data)
        (if (= fmt 0) (values (u16 d (+ o 2)) (+ o 4)) (values (u32 d (+ o 2)) (+ o 6)))
      (let* ((idx (min gid (1- mapcount)))
             (esz (1+ (ash (logand ef #x30) -4)))
             (ibits (1+ (logand ef #x0f)))
             (entry 0))
        (dotimes (b esz) (setf entry (logior (ash entry 8) (u8 d (+ data (* idx esz) b)))))
        (values (ash entry (- ibits)) (logand entry (1- (ash 1 ibits))))))))

(defun hvar-advance-delta (font gid norm)
  "HVAR advance-width delta for GID at NORM (0 if no HVAR)."
  (let ((o (font-table font "HVAR")))
    (if (null o) 0
        (let* ((d (font-data font)) (ivs (+ o (u32 d (+ o 4)))) (mapoff (u32 d (+ o 8))))
          (multiple-value-bind (outer inner)
              (if (zerop mapoff) (values 0 gid) (%delta-set-index d (+ o mapoff) gid))
            (%ivs-delta d ivs outer inner norm))))))

(defun default-norm (font)
  "All-zero normalized location (the default instance), or NIL if not variable."
  (let ((axes (font-fvar font)))
    (when axes (make-array (length axes) :element-type 'double-float :initial-element 0d0))))

(defun advance-at (font gid &optional norm)
  "Advance (font units) for GID at design-space NORM: hmtx + variation delta.
   Prefers HVAR (canonical) over gvar phantom points; HVAR applies even at the
   default location (some fonts, e.g. Apple's New York, have nonzero HVAR there)."
  (let ((base (glyph-advance font gid)))
    (cond ((font-table font "HVAR")
           (+ base (hvar-advance-delta font gid (or norm (default-norm font)))))
          ((and norm (font-table font "gvar")) (varied-advance font gid base norm))
          (t base))))
