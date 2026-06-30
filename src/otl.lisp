;;;; otl.lisp — OpenType Layout: GSUB/GPOS shaping.  STRONG-TIER framework.
;;;;
;;;; The coupled engine: Coverage/ClassDef parsing, Script/Feature/Lookup
;;;; navigation, the glyph buffer, and the apply loop. Two reference lookups are
;;;; implemented here (GPOS type 2 pair = kerning; GSUB type 4 = ligatures); the
;;;; remaining lookup *formats* are the W4 swarm surface (HarfBuzz oracle).
(in-package #:scribe)

;;; ---- Coverage / ClassDef ----
(defun parse-coverage (d o)
  "Return a hash-table glyph-id -> coverage-index."
  (let ((ht (make-hash-table :test 'eql)) (fmt (u16 d o)))
    (ecase fmt
      (1 (let ((n (u16 d (+ o 2))))
           (dotimes (i n) (setf (gethash (u16 d (+ o 4 (* i 2))) ht) i))))
      (2 (let ((n (u16 d (+ o 2))))
           (dotimes (i n)
             (let* ((r (+ o 4 (* i 6))) (start (u16 d r)) (end (u16 d (+ r 2)))
                    (base (u16 d (+ r 4))))
               (loop for g from start to end for k from 0
                     do (setf (gethash g ht) (+ base k))))))))
    ht))

(defun parse-classdef (d o)
  "Return a hash-table glyph-id -> class (absent => class 0)."
  (let ((ht (make-hash-table :test 'eql)) (fmt (u16 d o)))
    (ecase fmt
      (1 (let ((start (u16 d (+ o 2))) (n (u16 d (+ o 4))))
           (dotimes (i n) (setf (gethash (+ start i) ht) (u16 d (+ o 6 (* i 2)))))))
      (2 (let ((n (u16 d (+ o 2))))
           (dotimes (i n)
             (let ((r (+ o 4 (* i 6))))
               (loop for g from (u16 d r) to (u16 d (+ r 2))
                     do (setf (gethash g ht) (u16 d (+ r 4)))))))))
    ht))

;;; ---- ValueRecord ----
(defun value-size (fmt) (* 2 (logcount fmt)))
(defun read-value (d o fmt)
  "Read a ValueRecord -> (values xPlacement yPlacement xAdvance yAdvance). Device
   tables (bits 0x10..0x80) are skipped."
  (let ((p o) (xp 0) (yp 0) (xa 0) (ya 0))
    (flet ((nx () (prog1 (s16 d p) (incf p 2))))
      (when (logbitp 0 fmt) (setf xp (nx)))
      (when (logbitp 1 fmt) (setf yp (nx)))
      (when (logbitp 2 fmt) (setf xa (nx)))
      (when (logbitp 3 fmt) (setf ya (nx)))
      (dotimes (b 4) (when (logbitp (+ 4 b) fmt) (incf p 2))))
    (values xp yp xa ya)))

;;; ---- Script / Feature / Lookup navigation ----
(defun otl-lookup-offsets (font tag feature-tags)
  "For GPOS/GSUB table TAG, return (values lookup-list-base sorted-lookup-offsets)
   for the lookups referenced by FEATURE-TAGS under the default (latn/DFLT) script."
  (let ((base (font-table font tag)))
    (unless base (return-from otl-lookup-offsets (values nil nil)))
    (let* ((d (font-data font))
           (slist (+ base (u16 d (+ base 4))))
           (flist (+ base (u16 d (+ base 6))))
           (llist (+ base (u16 d (+ base 8))))
           (want (make-hash-table :test 'equal)))
      (dolist (ft feature-tags) (setf (gethash ft want) t))
      ;; choose script: latn else DFLT else first
      (let* ((nscr (u16 d slist)) (script-off nil) (dflt nil) (first-off nil))
        (dotimes (i nscr)
          (let* ((r (+ slist 2 (* i 6))) (stag (tag d r)) (so (+ slist (u16 d (+ r 4)))))
            (when (null first-off) (setf first-off so))
            (cond ((string= stag "latn") (setf script-off so))
                  ((string= stag "DFLT") (setf dflt so)))))
        (let ((script (or script-off dflt first-off)))
          (unless script (return-from otl-lookup-offsets (values llist nil)))
          ;; default langsys -> feature indices
          (let* ((dls (u16 d script))
                 (langsys (if (plusp dls) (+ script dls) nil))
                 (lookup-set (make-hash-table :test 'eql)))
            (when langsys
              (let ((nfeat (u16 d (+ langsys 4))))
                (dotimes (i nfeat)
                  (let* ((fidx (u16 d (+ langsys 6 (* i 2))))
                         (fr (+ flist 2 (* fidx 6))) (ftag (tag d fr))
                         (foff (+ flist (u16 d (+ fr 4)))))
                    (when (gethash ftag want)
                      (let ((nl (u16 d (+ foff 2))))
                        (dotimes (k nl) (setf (gethash (u16 d (+ foff 4 (* k 2))) lookup-set) t))))))))
            ;; lookup indices -> offsets, sorted by index (OT apply order)
            (let ((idxs (sort (loop for k being the hash-keys of lookup-set collect k) #'<)))
              (values llist
                      (mapcar (lambda (i) (+ llist (u16 d (+ llist 2 (* i 2))))) idxs)))))))))

;;; ---- the glyph buffer ----
(defun itemize (font text)
  "Text -> vector of glyph-pos with cmap gids + default hmtx advances (font units)."
  (let ((buf (make-array (length text) :adjustable t :fill-pointer 0)))
    (loop for ch across text for i from 0 do
      (let ((gid (font-glyph-index font (char-code ch))))
        (vector-push-extend
         (make-glyph-pos :gid gid :x-advance (glyph-advance font gid)
                         :y-advance 0 :x-offset 0 :y-offset 0 :cluster i) buf)))
    buf))

;;; ---- lookup appliers (extension-unwrapping) ----
(defun apply-lookup (font d lookup-off buf gpos)
  (let* ((type (u16 d lookup-off)) (nsub (u16 d (+ lookup-off 4))))
    (dotimes (s nsub)
      (let ((sub (+ lookup-off (u16 d (+ lookup-off 6 (* s 2)))) ) (rtype type))
        ;; extension (GSUB 7 / GPOS 9): unwrap
        (when (or (and gpos (= type 9)) (and (not gpos) (= type 7)))
          (setf rtype (u16 d (+ sub 2)) sub (+ sub (u32 d (+ sub 4)))))
        (if gpos
            (when (= rtype 2) (apply-pair-pos d sub buf))
            (when (= rtype 4) (apply-ligature d sub buf)))))))

(defun apply-pair-pos (d o buf)
  "GPOS LookupType 2 (pair adjustment), formats 1 & 2 -> kerning/positioning."
  (let* ((fmt (u16 d o)) (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (vf1 (u16 d (+ o 4))) (vf2 (u16 d (+ o 6))))
    (loop for i from 0 below (1- (fill-pointer buf)) do
      (let* ((g1 (glyph-pos-gid (aref buf i))) (ci (gethash g1 cov)))
        (when ci
          (let ((g2 (glyph-pos-gid (aref buf (1+ i)))))
            (ecase fmt
              (1 (let* ((ps (+ o (u16 d (+ o 10 (* ci 2)))))
                        (n (u16 d ps)) (rsz (+ 2 (value-size vf1) (value-size vf2))))
                   (dotimes (k n)
                     (let ((r (+ ps 2 (* k rsz))))
                       (when (= (u16 d r) g2)
                         (%apply-values d (+ r 2) vf1 vf2 buf i)
                         (return))))))
              (2 (let* ((cd1 (parse-classdef d (+ o (u16 d (+ o 8)))))
                        (cd2 (parse-classdef d (+ o (u16 d (+ o 10)))))
                        (c2count (u16 d (+ o 14)))
                        (cl1 (gethash g1 cd1 0)) (cl2 (gethash g2 cd2 0))
                        (rsz (+ (value-size vf1) (value-size vf2)))
                        (r (+ o 16 (* (+ (* cl1 c2count) cl2) rsz))))
                   (%apply-values d r vf1 vf2 buf i))))))))))

(defun %apply-values (d r vf1 vf2 buf i)
  (multiple-value-bind (xp1 yp1 xa1 ya1) (read-value d r vf1)
    (incf (glyph-pos-x-offset (aref buf i)) xp1)
    (incf (glyph-pos-y-offset (aref buf i)) yp1)
    (incf (glyph-pos-x-advance (aref buf i)) xa1)
    (incf (glyph-pos-y-advance (aref buf i)) ya1)
    (when (plusp vf2)
      (multiple-value-bind (xp2 yp2 xa2 ya2) (read-value d (+ r (value-size vf1)) vf2)
        (incf (glyph-pos-x-offset (aref buf (1+ i))) xp2)
        (incf (glyph-pos-y-offset (aref buf (1+ i))) yp2)
        (incf (glyph-pos-x-advance (aref buf (1+ i))) xa2)
        (incf (glyph-pos-y-advance (aref buf (1+ i))) ya2)))))

(defun apply-ligature (d o buf)
  "GSUB LookupType 4 (ligature substitution)."
  (let ((cov (parse-coverage d (+ o (u16 d (+ o 2))))) (nset (u16 d (+ o 4))))
    (declare (ignore nset))
    (let ((i 0))
      (loop while (< i (fill-pointer buf)) do
        (let* ((g1 (glyph-pos-gid (aref buf i))) (ci (gethash g1 cov)))
          (if (null ci) (incf i)
              (let* ((ls (+ o (u16 d (+ o 6 (* ci 2))))) (nlig (u16 d ls)) (done nil))
                (dotimes (k nlig)
                  (let* ((lig (+ ls (u16 d (+ ls 2 (* k 2)))))
                         (lg (u16 d lig)) (comp (u16 d (+ lig 2))))
                    (when (<= (+ i comp) (fill-pointer buf))
                      (let ((match t))
                        (loop for c from 1 below comp
                              unless (= (glyph-pos-gid (aref buf (+ i c))) (u16 d (+ lig 2 (* c 2))))
                                do (setf match nil) (return))
                        (when match
                          (setf (glyph-pos-gid (aref buf i)) lg)
                          ;; remove the (comp-1) trailing components
                          (loop for j from (+ i 1) below (- (fill-pointer buf) (1- comp))
                                do (setf (aref buf j) (aref buf (+ j (1- comp)))))
                          (decf (fill-pointer buf) (1- comp))
                          (setf done t) (return))))))
                (unless done (incf i)))))))))

;;; ---- the public entry point ----
(defun shape-run (font text &key (features '(:liga :calt :kern))
                                 (script :latn) (direction :ltr) variation)
  "Shape TEXT with FONT -> vector of glyph-pos (gid + advances/offsets, font units)."
  (declare (ignore script direction variation))
  (let* ((d (font-data font))
         (gsub-tags (remove nil (mapcar (lambda (f) (case f (:liga "liga") (:calt "calt")
                                                      (:dlig "dlig") (:rlig "rlig"))) features)))
         (gpos-tags (remove nil (mapcar (lambda (f) (case f (:kern "kern") (:mark "mark")
                                                      (:mkmk "mkmk"))) features)))
         (buf (itemize font text)))
    (when gsub-tags
      (multiple-value-bind (_ lks) (otl-lookup-offsets font "GSUB" gsub-tags)
        (declare (ignore _)) (dolist (lk lks) (apply-lookup font d lk buf nil))))
    ;; advances follow the FINAL gids (GSUB may have substituted ligatures)
    (loop for g across buf
          do (setf (glyph-pos-x-advance g) (glyph-advance font (glyph-pos-gid g))))
    (when gpos-tags
      (multiple-value-bind (_ lks) (otl-lookup-offsets font "GPOS" gpos-tags)
        (declare (ignore _)) (dolist (lk lks) (apply-lookup font d lk buf t))))
    buf))
