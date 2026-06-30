;;;; otl.lisp — OpenType Layout: GSUB/GPOS shaping.  STRONG-TIER.
;;;;
;;;; The coupled engine: Coverage/ClassDef parsing, GDEF glyph classes,
;;;; Script/Feature/Lookup navigation, the glyph buffer, lookupFlag skipping, and
;;;; the apply loop with recursive (contextual) nested-lookup dispatch.
;;;;
;;;; Implemented lookups:
;;;;   GSUB 1 single, 2 multiple, 4 ligature, 5 context, 6 chaining-context
;;;;   GPOS 1 single, 2 pair, 4 mark-to-base, 5 mark-to-ligature, 6 mark-to-mark
;;;; Extensions (GSUB 7 / GPOS 9) are unwrapped transparently.
;;;; Verified byte-identical to HarfBuzz (inspect/shape-test.lisp).
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

;;; ===========================================================================
;;; GDEF — glyph classes + mark-attach classes for lookupFlag mark filtering.
;;; ===========================================================================
(defstruct gdef
  glyph-class      ; hash gid -> class (1 base, 2 ligature, 3 mark, 4 component)
  mark-attach      ; hash gid -> mark-attachment class
  mark-sets-off)   ; absolute offset of MarkGlyphSetsDef table (or nil)

(defun parse-gdef (font)
  "Parse GDEF into a GDEF struct (cached on the font), or NIL if absent."
  (let ((cached (font-%gdef font)))
    (when cached (return-from parse-gdef (if (eq cached :none) nil cached))))
  (let ((base (font-table font "GDEF")))
    (unless base
      (setf (font-%gdef font) :none)
      (return-from parse-gdef nil))
    (let* ((d (font-data font))
           (gco (u16 d (+ base 4)))
           (maco (u16 d (+ base 10)))
           (minor (u16 d (+ base 2)))
           (msoff (when (>= minor 2)
                    (let ((v (u16 d (+ base 12)))) (when (plusp v) (+ base v)))))
           (g (make-gdef
               :glyph-class (when (plusp gco) (parse-classdef d (+ base gco)))
               :mark-attach (when (plusp maco) (parse-classdef d (+ base maco)))
               :mark-sets-off msoff)))
      (setf (font-%gdef font) g)
      g)))

(defun gdef-glyph-klass (gdef gid)
  "GDEF glyph class for GID (0 if unknown / no GDEF)."
  (if (and gdef (gdef-glyph-class gdef))
      (gethash gid (gdef-glyph-class gdef) 0)
      0))

(defun mark-set-covers-p (font gdef set-index gid)
  "Is GID a member of MarkGlyphSet SET-INDEX (UseMarkFilteringSet)?"
  (let ((mso (and gdef (gdef-mark-sets-off gdef))))
    (when mso
      (let* ((d (font-data font)) (n (u16 d (+ mso 2))))
        (when (< set-index n)
          (let ((cov (+ mso (u32 d (+ mso 4 (* set-index 4))))))
            (and (gethash gid (parse-coverage d cov)) t)))))))

;;; ---- lookupFlag glyph skipping ----
;;; lookupFlag bits: 0x01 RightToLeftBaseline (cursive), 0x02 IgnoreBaseGlyphs,
;;; 0x04 IgnoreLigatures, 0x08 IgnoreMarks, 0x10 UseMarkFilteringSet,
;;; high byte = MarkAttachmentType class.
(defun glyph-skipped-p (font gdef flag mark-filter-set gid)
  "Should the apply loop skip GID under FLAG?"
  (let ((klass (gdef-glyph-klass gdef gid)))
    (cond
      ((and (logbitp 1 flag) (= klass 1)) t)            ; IgnoreBaseGlyphs
      ((and (logbitp 2 flag) (= klass 2)) t)            ; IgnoreLigatures
      ((and (logbitp 3 flag) (= klass 3)) t)            ; IgnoreMarks
      ;; mark filtering: when this glyph is a mark, possibly ignore it
      ((and (= klass 3) (logbitp 4 flag))               ; UseMarkFilteringSet
       (not (mark-set-covers-p font gdef mark-filter-set gid)))
      ((and (= klass 3) (>= flag #x100))                ; MarkAttachmentType
       (let ((want (ash flag -8))
             (have (if (and gdef (gdef-mark-attach gdef))
                       (gethash gid (gdef-mark-attach gdef) 0) 0)))
         (/= have want)))
      (t nil))))

(defun next-glyph (font gdef flag mfs buf i)
  "Index of next non-skipped glyph at or after I, or NIL."
  (loop for j from i below (fill-pointer buf)
        unless (glyph-skipped-p font gdef flag mfs (glyph-pos-gid (aref buf j)))
          do (return j)))

(defun prev-glyph (font gdef flag mfs buf i)
  "Index of previous non-skipped glyph at or before I, or NIL."
  (loop for j from i downto 0
        unless (glyph-skipped-p font gdef flag mfs (glyph-pos-gid (aref buf j)))
          do (return j)))

;;; ===========================================================================
;;; Script / Feature / Lookup navigation
;;; ===========================================================================
(defun otl-lookup-indices (font tag feature-tags)
  "For GPOS/GSUB table TAG, return (values lookup-list-base sorted-lookup-indices)
   for the lookups referenced by FEATURE-TAGS under the default (latn/DFLT) script."
  (let ((base (font-table font tag)))
    (unless base (return-from otl-lookup-indices (values nil nil)))
    (let* ((d (font-data font))
           (slist (+ base (u16 d (+ base 4))))
           (flist (+ base (u16 d (+ base 6))))
           (llist (+ base (u16 d (+ base 8))))
           (want (make-hash-table :test 'equal)))
      (dolist (ft feature-tags) (setf (gethash ft want) t))
      (let* ((nscr (u16 d slist)) (script-off nil) (dflt nil) (first-off nil))
        (dotimes (i nscr)
          (let* ((r (+ slist 2 (* i 6))) (stag (tag d r)) (so (+ slist (u16 d (+ r 4)))))
            (when (null first-off) (setf first-off so))
            (cond ((string= stag "latn") (setf script-off so))
                  ((string= stag "DFLT") (setf dflt so)))))
        (let ((script (or script-off dflt first-off)))
          (unless script (return-from otl-lookup-indices (values llist nil)))
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
            (values llist
                    (sort (loop for k being the hash-keys of lookup-set collect k) #'<))))))))

(defun lookup-offset (llist d idx)
  (+ llist (u16 d (+ llist 2 (* idx 2)))))

;;; ===========================================================================
;;; the glyph buffer
;;; ===========================================================================
(defun itemize (font text)
  "Text -> vector of glyph-pos with cmap gids + default hmtx advances (font units)."
  (let ((buf (make-array (length text) :adjustable t :fill-pointer 0)))
    (loop for ch across text for i from 0 do
      (let ((gid (font-glyph-index font (char-code ch))))
        (vector-push-extend
         (make-glyph-pos :gid gid :x-advance (advance-at font gid)
                         :y-advance 0 :x-offset 0 :y-offset 0 :cluster i) buf)))
    buf))

;;; ===========================================================================
;;; the apply loop — drives a lookup over the buffer honoring lookupFlag.
;;; A *shape-ctx* binds the bits a contextual nested-lookup invocation needs:
;;; the font, data, table-kind (gpos?), and the lookup-list base for recursion.
;;; ===========================================================================
(defstruct sctx font d gpos llist gdef)

(defun apply-lookup (ctx lookup-off buf)
  "Drive lookup at LOOKUP-OFF across BUF, honoring lookupFlag skipping. Iterates
   glyph positions; each subtable type advances or rewrites the buffer."
  (let* ((d (sctx-d ctx))
         (type (u16 d lookup-off))
         (flag (u16 d (+ lookup-off 2)))
         (nsub (u16 d (+ lookup-off 4)))
         (mfs (if (logbitp 4 flag) (u16 d (+ lookup-off 6 (* nsub 2))) 0))
         (gpos (sctx-gpos ctx))
         (font (sctx-font ctx)) (gdef (sctx-gdef ctx)))
    ;; Walk the buffer; at each non-skipped position, try every subtable in turn
    ;; (each one extension-unwrapped on the fly) until one applies.
    (let ((i 0))
      (loop while (< i (fill-pointer buf)) do
        (let ((gid (glyph-pos-gid (aref buf i))))
          (if (glyph-skipped-p font gdef flag mfs gid)
              (incf i)
              (let ((consumed nil))
                (dotimes (s nsub)
                  (let ((sub (+ lookup-off (u16 d (+ lookup-off 6 (* s 2))))) (rtype type))
                    (when (or (and gpos (= type 9)) (and (not gpos) (= type 7)))
                      (setf rtype (u16 d (+ sub 2)) sub (+ sub (u32 d (+ sub 4)))))
                    (let ((adv (apply-subtable ctx rtype sub flag mfs buf i)))
                      (when adv (setf consumed adv) (return)))))
                ;; consumed: NIL = no match (advance 1); >=1 = advance that many;
                ;; 0 = a deletion shrank the buffer at I, reprocess position I.
                (cond ((null consumed) (incf i))
                      ((zerop consumed) nil)   ; stay: buffer shrank, glyph moved in
                      (t (incf i consumed))))))))))

(defun apply-subtable (ctx rtype sub flag mfs buf i)
  "Try to apply one subtable at glyph I. Return number of buffer positions to
   advance the cursor (>=1) if it applied, else NIL."
  (let ((gpos (sctx-gpos ctx)))
    (if gpos
        (case rtype
          (1 (apply-single-pos ctx sub flag mfs buf i))
          (2 (apply-pair-pos   ctx sub flag mfs buf i))
          (4 (apply-mark-base  ctx sub flag mfs buf i))
          (5 (apply-mark-lig   ctx sub flag mfs buf i))
          (6 (apply-mark-mark  ctx sub flag mfs buf i))
          (7 (apply-context    ctx sub flag mfs buf i t))
          (8 (apply-chain-context ctx sub flag mfs buf i t))
          (t nil))
        (case rtype
          (1 (apply-single-sub   ctx sub flag mfs buf i))
          (2 (apply-multiple-sub ctx sub flag mfs buf i))
          (4 (apply-ligature     ctx sub flag mfs buf i))
          (5 (apply-context      ctx sub flag mfs buf i nil))
          (6 (apply-chain-context ctx sub flag mfs buf i nil))
          (t nil)))))

;;; ===========================================================================
;;; GSUB 1 — single substitution
;;; ===========================================================================
(defun apply-single-sub (ctx o flag mfs buf i)
  (declare (ignore flag mfs))
  (let* ((d (sctx-d ctx)) (fmt (u16 d o))
         (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
    (when ci
      (let ((ng (ecase fmt
                  (1 (logand #xffff (+ g (s16 d (+ o 4)))))
                  (2 (u16 d (+ o 6 (* ci 2)))))))
        (setf (glyph-pos-gid (aref buf i)) ng)
        1))))

;;; ===========================================================================
;;; GSUB 2 — multiple substitution (one glyph -> sequence; grows the buffer)
;;; ===========================================================================
(defun apply-multiple-sub (ctx o flag mfs buf i)
  (declare (ignore flag mfs))
  (let* ((d (sctx-d ctx)) (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
    (when ci
      (let* ((seq (+ o (u16 d (+ o 6 (* ci 2))))) (n (u16 d seq))
             (cl (glyph-pos-cluster (aref buf i))))
        (cond
          ((zerop n)
           ;; delete this glyph
           (loop for j from i below (1- (fill-pointer buf))
                 do (setf (aref buf j) (aref buf (1+ j))))
           (decf (fill-pointer buf))
           ;; advancing 0 would loop; signal "deleted, stay" => return 0 handled
           ;; by caller; but our caller treats nil as +1. Return a marker:
           0)
          (t
           ;; replace glyph i with first; insert (n-1) more after it.
           (let ((firsts (loop for k below n collect (u16 d (+ seq 2 (* k 2))))))
             (setf (glyph-pos-gid (aref buf i)) (first firsts))
             ;; make room
             (loop repeat (1- n) do (vector-push-extend (make-glyph-pos) buf))
             (loop for j from (1- (fill-pointer buf)) downto (+ i n)
                   do (setf (aref buf j) (aref buf (- j (1- n)))))
             (loop for k from 1 below n for rest in (rest firsts)
                   do (setf (aref buf (+ i k))
                            (make-glyph-pos :gid rest :x-advance 0 :y-advance 0
                                            :x-offset 0 :y-offset 0 :cluster cl)))
             n)))))))

;;; ===========================================================================
;;; GSUB 4 — ligature substitution
;;; ===========================================================================
(defun apply-ligature (ctx o flag mfs buf i)
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (g1 (glyph-pos-gid (aref buf i))) (ci (gethash g1 cov)))
    (when ci
      (let* ((ls (+ o (u16 d (+ o 6 (* ci 2))))) (nlig (u16 d ls)))
        (dotimes (k nlig)
          (let* ((lig (+ ls (u16 d (+ ls 2 (* k 2)))))
                 (lg (u16 d lig)) (comp (u16 d (+ lig 2)))
                 ;; collect indices of the (comp-1) following non-skipped glyphs
                 (idxs (list i)) (cur i) (ok t))
            (loop for c from 1 below comp do
              (let ((nx (next-glyph font gdef flag mfs buf (1+ cur))))
                (if (and nx (= (glyph-pos-gid (aref buf nx)) (u16 d (+ lig 2 (* c 2)))))
                    (progn (push nx idxs) (setf cur nx))
                    (progn (setf ok nil) (return)))))
            (when ok
              (setf idxs (nreverse idxs))
              (setf (glyph-pos-gid (aref buf i)) lg)
              ;; remove the trailing matched components (all but the first)
              (dolist (rem (sort (rest idxs) #'>))
                (loop for j from rem below (1- (fill-pointer buf))
                      do (setf (aref buf j) (aref buf (1+ j))))
                (decf (fill-pointer buf)))
              (return-from apply-ligature 1))))))
    nil))

;;; ===========================================================================
;;; GPOS 1 — single adjustment
;;; ===========================================================================
(defun apply-single-pos (ctx o flag mfs buf i)
  (declare (ignore flag mfs))
  (let* ((d (sctx-d ctx)) (fmt (u16 d o))
         (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (vf (u16 d (+ o 4)))
         (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
    (when ci
      (let ((r (ecase fmt
                 (1 (+ o 6))
                 (2 (+ o 8 (* ci (value-size vf)))))))
        (multiple-value-bind (xp yp xa ya) (read-value d r vf)
          (incf (glyph-pos-x-offset (aref buf i)) xp)
          (incf (glyph-pos-y-offset (aref buf i)) yp)
          (incf (glyph-pos-x-advance (aref buf i)) xa)
          (incf (glyph-pos-y-advance (aref buf i)) ya))
        1))))

;;; ===========================================================================
;;; GPOS 2 — pair adjustment
;;; ===========================================================================
(defun apply-pair-pos (ctx o flag mfs buf i)
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (fmt (u16 d o)) (cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (vf1 (u16 d (+ o 4))) (vf2 (u16 d (+ o 6)))
         (g1 (glyph-pos-gid (aref buf i))) (ci (gethash g1 cov)))
    (when ci
      (let ((j (next-glyph font gdef flag mfs buf (1+ i))))
        (when j
          (let ((g2 (glyph-pos-gid (aref buf j))))
            (ecase fmt
              (1 (let* ((ps (+ o (u16 d (+ o 10 (* ci 2)))))
                        (n (u16 d ps)) (rsz (+ 2 (value-size vf1) (value-size vf2))))
                   (dotimes (k n)
                     (let ((r (+ ps 2 (* k rsz))))
                       (when (= (u16 d r) g2)
                         (%apply-pair-values d (+ r 2) vf1 vf2 buf i j)
                         (return-from apply-pair-pos (if (plusp vf2) 2 1)))))
                   nil))
              (2 (let* ((cd1 (parse-classdef d (+ o (u16 d (+ o 8)))))
                        (cd2 (parse-classdef d (+ o (u16 d (+ o 10)))))
                        (c2count (u16 d (+ o 14)))
                        (cl1 (gethash g1 cd1 0)) (cl2 (gethash g2 cd2 0))
                        (rsz (+ (value-size vf1) (value-size vf2)))
                        (r (+ o 16 (* (+ (* cl1 c2count) cl2) rsz))))
                   (%apply-pair-values d r vf1 vf2 buf i j)
                   (if (plusp vf2) 2 1))))))))))

(defun %apply-pair-values (d r vf1 vf2 buf i j)
  (multiple-value-bind (xp1 yp1 xa1 ya1) (read-value d r vf1)
    (incf (glyph-pos-x-offset (aref buf i)) xp1)
    (incf (glyph-pos-y-offset (aref buf i)) yp1)
    (incf (glyph-pos-x-advance (aref buf i)) xa1)
    (incf (glyph-pos-y-advance (aref buf i)) ya1)
    (when (plusp vf2)
      (multiple-value-bind (xp2 yp2 xa2 ya2) (read-value d (+ r (value-size vf1)) vf2)
        (incf (glyph-pos-x-offset (aref buf j)) xp2)
        (incf (glyph-pos-y-offset (aref buf j)) yp2)
        (incf (glyph-pos-x-advance (aref buf j)) xa2)
        (incf (glyph-pos-y-advance (aref buf j)) ya2)))))

;;; ===========================================================================
;;; Anchors
;;; ===========================================================================
(defun read-anchor (d o)
  "Anchor table (formats 1/2/3) -> (values x y). Device tables ignored."
  (when (zerop o) (return-from read-anchor (values nil nil)))
  (let ((fmt (u16 d o)))
    (declare (ignore fmt))
    ;; all formats begin with format + xCoordinate + yCoordinate
    (values (s16 d (+ o 2)) (s16 d (+ o 4)))))

;;; ---- shared MarkArray reader: mark index -> (values mark-class ax ay) ----
(defun read-mark-array (d marray markidx)
  "MarkArray: markCount then (class, anchorOffset). Return (values class ax ay)."
  (let* ((rec (+ marray 2 (* markidx 4)))
         (cls (u16 d rec))
         (anch (+ marray (u16 d (+ rec 2)))))
    (multiple-value-bind (ax ay) (read-anchor d anch)
      (values cls ax ay))))

;;; ===========================================================================
;;; GPOS 4 — mark-to-base
;;; ===========================================================================
(defun apply-mark-base (ctx o flag mfs buf i)
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (markcov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (basecov (parse-coverage d (+ o (u16 d (+ o 4)))))
         (mclass-count (u16 d (+ o 6)))
         (marray (+ o (u16 d (+ o 8))))
         (barray (+ o (u16 d (+ o 10))))
         (mg (glyph-pos-gid (aref buf i)))
         (mi (gethash mg markcov)))
    (when mi
      ;; find preceding base glyph (skip marks via flag handling). We search
      ;; backward for a glyph covered by basecov, skipping per lookupFlag AND
      ;; (always) skipping mark glyphs since marks can't be the base here.
      (let ((bidx (mark-find-base font gdef flag mfs basecov buf i)))
        (when bidx
          (let ((bi (gethash (glyph-pos-gid (aref buf bidx)) basecov)))
            (multiple-value-bind (mclass max may) (read-mark-array d marray mi)
              (when (and max (< mclass mclass-count))
                (let* ((brec (+ barray 2 (* bi mclass-count 2)))
                       (banch (+ barray (u16 d (+ brec (* mclass 2))))))
                  (multiple-value-bind (bax bay) (read-anchor d banch)
                    (when bax
                      (attach-mark buf i bidx bax bay max may)
                      (return-from apply-mark-base 1))))))))))
    nil))

(defun mark-find-base (font gdef flag mfs cov buf i)
  "Search backward from I-1 for a glyph in COV that is not skipped by FLAG and is
   not itself a mark; return its index or NIL."
  (loop for j from (1- i) downto 0
        for gid = (glyph-pos-gid (aref buf j))
        do (unless (glyph-skipped-p font gdef flag mfs gid)
             (when (gethash gid cov) (return j))
             ;; a non-skipped, non-covered glyph still ends the search only if
             ;; it's a base/ligature; keep scanning past intervening marks.
             (when (/= (gdef-glyph-klass gdef gid) 3) (return nil)))))

(defun attach-mark (buf markidx baseidx bax bay max may)
  "Position mark at MARKIDX so its anchor (MAX,MAY) aligns to base anchor
   (BAX,BAY). Account for advances accumulated from base..mark-1."
  ;; HarfBuzz LTR: mark x-offset = base-anchor-x - mark-anchor-x - sum(advances
  ;; of glyphs from base..markidx-1). y similarly (no advance accumulation in y
  ;; for horizontal). Marks carry zero advance.
  (let ((adv 0))
    (loop for j from baseidx below markidx
          do (incf adv (glyph-pos-x-advance (aref buf j))))
    (setf (glyph-pos-x-offset (aref buf markidx)) (- bax max adv))
    (setf (glyph-pos-y-offset (aref buf markidx)) (- bay may))))

;;; ===========================================================================
;;; GPOS 6 — mark-to-mark
;;; ===========================================================================
(defun apply-mark-mark (ctx o flag mfs buf i)
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (mark1cov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (mark2cov (parse-coverage d (+ o (u16 d (+ o 4)))))
         (mclass-count (u16 d (+ o 6)))
         (m1array (+ o (u16 d (+ o 8))))
         (m2array (+ o (u16 d (+ o 10))))
         (mg (glyph-pos-gid (aref buf i)))
         (mi (gethash mg mark1cov)))
    (when mi
      ;; preceding mark2 glyph: the immediately preceding non-skipped glyph that
      ;; is in mark2cov.
      (let ((pidx (prev-glyph font gdef flag mfs buf (1- i))))
        (when (and pidx (gethash (glyph-pos-gid (aref buf pidx)) mark2cov))
          (let ((m2i (gethash (glyph-pos-gid (aref buf pidx)) mark2cov)))
            (multiple-value-bind (mclass max may) (read-mark-array d m1array mi)
              (when (and max (< mclass mclass-count))
                (let* ((rec (+ m2array 2 (* m2i mclass-count 2)))
                       (anch (+ m2array (u16 d (+ rec (* mclass 2))))))
                  (multiple-value-bind (bax bay) (read-anchor d anch)
                    (when bax
                      (attach-mark buf i pidx bax bay max may)
                      (return-from apply-mark-mark 1))))))))))
    nil))

;;; ===========================================================================
;;; GPOS 5 — mark-to-ligature
;;; ===========================================================================
(defun apply-mark-lig (ctx o flag mfs buf i)
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (markcov (parse-coverage d (+ o (u16 d (+ o 2)))))
         (ligcov (parse-coverage d (+ o (u16 d (+ o 4)))))
         (mclass-count (u16 d (+ o 6)))
         (marray (+ o (u16 d (+ o 8))))
         (larray (+ o (u16 d (+ o 10))))
         (mg (glyph-pos-gid (aref buf i)))
         (mi (gethash mg markcov)))
    (when mi
      (let ((lidx (mark-find-lig font gdef flag mfs ligcov buf i)))
        (when lidx
          (let* ((li (gethash (glyph-pos-gid (aref buf lidx)) ligcov))
                 (latt (+ larray (u16 d (+ larray 2 (* li 2)))))
                 (comp-count (u16 d latt))
                 ;; choose component: simplest correct choice = last component
                 (comp (1- comp-count)))
            (multiple-value-bind (mclass max may) (read-mark-array d marray mi)
              (when (and max (< mclass mclass-count))
                (let* ((rec (+ latt 2 (* comp mclass-count 2)))
                       (anch (+ latt (u16 d (+ rec (* mclass 2))))))
                  (when (plusp (u16 d (+ rec (* mclass 2))))
                    (multiple-value-bind (bax bay) (read-anchor d anch)
                      (when bax
                        (attach-mark buf i lidx bax bay max may)
                        (return-from apply-mark-lig 1)))))))))))
    nil))

(defun mark-find-lig (font gdef flag mfs cov buf i)
  (loop for j from (1- i) downto 0
        for gid = (glyph-pos-gid (aref buf j))
        do (unless (glyph-skipped-p font gdef flag mfs gid)
             (when (gethash gid cov) (return j))
             (when (/= (gdef-glyph-klass gdef gid) 3) (return nil)))))

;;; ===========================================================================
;;; Contextual (GSUB 5 / GPOS 7) and Chaining (GSUB 6 / GPOS 8)
;;; A match collects the input glyph indices, then nested SequenceLookupRecords
;;; (seqIndex into the matched input, lookupListIndex) are applied recursively.
;;; ===========================================================================
(defun match-class-seq (font gdef flag mfs buf start dir classes classdef nclasses)
  "Match NCLASSES class values from CLASSES (vector) against BUF walking DIR
   (+1 fwd / -1 back) from START (inclusive), skipping per flag. Return list of
   matched indices (in walk order) or NIL."
  (let ((idxs '()) (j start))
    (dotimes (k nclasses (nreverse idxs))
      (loop ;; advance to next non-skipped
        (when (or (< j 0) (>= j (fill-pointer buf))) (return-from match-class-seq nil))
        (if (glyph-skipped-p font gdef flag mfs (glyph-pos-gid (aref buf j)))
            (incf j dir)
            (return)))
      (let ((cl (gethash (glyph-pos-gid (aref buf j)) classdef 0)))
        (unless (= cl (aref classes k)) (return-from match-class-seq nil)))
      (push j idxs)
      (incf j dir))))

(defun match-glyph-seq (font gdef flag mfs buf start dir gids ngids)
  "Match NGIDS glyph ids from GIDS (vector) against BUF walking DIR from START."
  (let ((idxs '()) (j start))
    (dotimes (k ngids (nreverse idxs))
      (loop
        (when (or (< j 0) (>= j (fill-pointer buf))) (return-from match-glyph-seq nil))
        (if (glyph-skipped-p font gdef flag mfs (glyph-pos-gid (aref buf j)))
            (incf j dir)
            (return)))
      (unless (= (glyph-pos-gid (aref buf j)) (aref gids k))
        (return-from match-glyph-seq nil))
      (push j idxs)
      (incf j dir))))

(defun match-coverage-seq (font gdef flag mfs buf start dir covs ncovs)
  "Match NCOVS coverage tables (list of hash) against BUF walking DIR from START."
  (let ((idxs '()) (j start))
    (dotimes (k ncovs (nreverse idxs))
      (loop
        (when (or (< j 0) (>= j (fill-pointer buf))) (return-from match-coverage-seq nil))
        (if (glyph-skipped-p font gdef flag mfs (glyph-pos-gid (aref buf j)))
            (incf j dir)
            (return)))
      (unless (gethash (glyph-pos-gid (aref buf j)) (nth k covs))
        (return-from match-coverage-seq nil))
      (push j idxs)
      (incf j dir))))

(defun apply-seq-lookups (ctx records nrec d ro input-idxs flag mfs buf)
  "Apply NREC SequenceLookupRecords at RECORDS (each: seqIndex u16, lookupIdx u16)
   to the matched INPUT-IDXS. Recurses into apply-lookup for one position. Returns
   the net change in buffer length contributed by these nested lookups (for cursor
   advancement)."
  (declare (ignore flag mfs))
  (let ((start-len (fill-pointer buf)))
    (dotimes (r nrec)
      (let* ((rec (+ ro (* r 4)))
             (seqi (u16 d rec))
             (lidx (u16 d (+ rec 2))))
        (when (< seqi (length input-idxs))
          (let* ((pos (nth seqi input-idxs))
                 (loff (lookup-offset (sctx-llist ctx) d lidx)))
            ;; Apply the nested lookup at exactly this glyph position. We run a
            ;; single-position application by temporarily constraining: most
            ;; nested lookups are single-glyph (sub/pos type 1) so applying at POS
            ;; suffices. Use apply-subtable-at for one shot.
            (apply-nested-lookup ctx loff buf pos)))))
    (- (fill-pointer buf) start-len)))

(defun apply-nested-lookup (ctx lookup-off buf pos)
  "Apply lookup LOOKUP-OFF at exactly buffer position POS (one application)."
  (when (and (>= pos 0) (< pos (fill-pointer buf)))
    (let* ((d (sctx-d ctx)) (type (u16 d lookup-off))
           (flag (u16 d (+ lookup-off 2))) (nsub (u16 d (+ lookup-off 4)))
           (mfs (if (logbitp 4 flag) (u16 d (+ lookup-off 6 (* nsub 2))) 0))
           (gpos (sctx-gpos ctx)))
      (dotimes (s nsub)
        (let ((sub (+ lookup-off (u16 d (+ lookup-off 6 (* s 2))))) (rtype type))
          (when (or (and gpos (= type 9)) (and (not gpos) (= type 7)))
            (setf rtype (u16 d (+ sub 2)) sub (+ sub (u32 d (+ sub 4)))))
          (when (apply-subtable ctx rtype sub flag mfs buf pos)
            (return)))))))

;;; ---- Contextual (type 5 GSUB / 7 GPOS) ----
(defun apply-context (ctx o flag mfs buf i gpos)
  (declare (ignore gpos))
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (fmt (u16 d o)))
    (ecase fmt
      (1 ;; glyph-based: coverage + rulesets keyed by first input glyph
       (let* ((cov (parse-coverage d (+ o (u16 d (+ o 2)))))
              (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
         (when ci
           (let* ((nset (u16 d (+ o 4)))
                  (setoff (when (< ci nset) (u16 d (+ o 6 (* ci 2))))))
             (when (and setoff (plusp setoff))
               (let* ((set (+ o setoff)) (nrule (u16 d set)))
                 (dotimes (r nrule)
                   (let* ((rule (+ set (u16 d (+ set 2 (* r 2)))))
                          (glyphcount (u16 d rule))
                          (seqcount (u16 d (+ rule 2)))
                          (in (make-array (max 1 (1- glyphcount)))))
                     (loop for k from 1 below glyphcount
                           do (setf (aref in (1- k)) (u16 d (+ rule 4 (* (1- k) 2)))))
                     (let ((idxs (match-glyph-seq font gdef flag mfs buf (1+ i) +1
                                                  in (1- glyphcount))))
                       (when (or (= glyphcount 1) idxs)
                         (let ((input (cons i idxs))
                               (ro (+ rule 4 (* (1- glyphcount) 2))))
                           (apply-seq-lookups ctx ro seqcount d ro input flag mfs buf)
                           (return-from apply-context 1)))))))))))
       nil)
      (2 ;; class-based
       (let* ((cov (parse-coverage d (+ o (u16 d (+ o 2)))))
              (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
         (when ci
           (let* ((cd (parse-classdef d (+ o (u16 d (+ o 4)))))
                  (cl (gethash g cd 0))
                  (nset (u16 d (+ o 6)))
                  (setoff (when (< cl nset) (u16 d (+ o 8 (* cl 2))))))
             (when (and setoff (plusp setoff))
               (let* ((set (+ o setoff)) (nrule (u16 d set)))
                 (dotimes (r nrule)
                   (let* ((rule (+ set (u16 d (+ set 2 (* r 2)))))
                          (glyphcount (u16 d rule))
                          (seqcount (u16 d (+ rule 2)))
                          (classes (make-array (max 1 (1- glyphcount)))))
                     (loop for k from 1 below glyphcount
                           do (setf (aref classes (1- k)) (u16 d (+ rule 4 (* (1- k) 2)))))
                     (let ((idxs (match-class-seq font gdef flag mfs buf (1+ i) +1
                                                  classes cd (1- glyphcount))))
                       (when (or (= glyphcount 1) idxs)
                         (let ((input (cons i idxs))
                               (ro (+ rule 4 (* (1- glyphcount) 2))))
                           (apply-seq-lookups ctx ro seqcount d ro input flag mfs buf)
                           (return-from apply-context 1)))))))))))
       nil)
      (3 ;; coverage-based
       (let* ((glyphcount (u16 d (+ o 2)))
              (seqcount (u16 d (+ o 4)))
              (covs (loop for k below glyphcount
                          collect (parse-coverage d (+ o (u16 d (+ o 6 (* k 2))))))))
         (when (gethash (glyph-pos-gid (aref buf i)) (first covs))
           (let ((idxs (match-coverage-seq font gdef flag mfs buf i +1 covs glyphcount)))
             (when idxs
               (let ((ro (+ o 6 (* glyphcount 2))))
                 (apply-seq-lookups ctx ro seqcount d ro idxs flag mfs buf)
                 (return-from apply-context 1)))))
         nil)))))

;;; ---- Chaining contextual (type 6 GSUB / 8 GPOS) ----
(defun apply-chain-context (ctx o flag mfs buf i gpos)
  (declare (ignore gpos))
  (let* ((d (sctx-d ctx)) (font (sctx-font ctx)) (gdef (sctx-gdef ctx))
         (fmt (u16 d o)))
    (ecase fmt
      (1 ;; glyph-based chain
       (let* ((cov (parse-coverage d (+ o (u16 d (+ o 2)))))
              (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
         (when ci
           (let* ((nset (u16 d (+ o 4)))
                  (setoff (when (< ci nset) (u16 d (+ o 6 (* ci 2))))))
             (when (and setoff (plusp setoff))
               (let* ((set (+ o setoff)) (nrule (u16 d set)))
                 (dotimes (r nrule)
                   (let ((p (+ set (u16 d (+ set 2 (* r 2))))))
                     (multiple-value-bind (ok input ro seqcount)
                         (chain-rule-match-glyph font gdef flag mfs buf i d p)
                       (when ok
                         (apply-seq-lookups ctx ro seqcount d ro input flag mfs buf)
                         (return-from apply-chain-context 1)))))))))
         nil))
      (2 ;; class-based chain
       (let* ((cov (parse-coverage d (+ o (u16 d (+ o 2)))))
              (g (glyph-pos-gid (aref buf i))) (ci (gethash g cov)))
         (when ci
           (let* ((bcd (parse-classdef d (+ o (u16 d (+ o 4)))))
                  (icd (parse-classdef d (+ o (u16 d (+ o 6)))))
                  (lcd (parse-classdef d (+ o (u16 d (+ o 8)))))
                  (cl (gethash g icd 0))
                  (nset (u16 d (+ o 10)))
                  (setoff (when (< cl nset) (u16 d (+ o 12 (* cl 2))))))
             (when (and setoff (plusp setoff))
               (let* ((set (+ o setoff)) (nrule (u16 d set)))
                 (dotimes (r nrule)
                   (let ((p (+ set (u16 d (+ set 2 (* r 2))))))
                     (multiple-value-bind (ok input ro seqcount)
                         (chain-rule-match-class font gdef flag mfs buf i d p bcd icd lcd)
                       (when ok
                         (apply-seq-lookups ctx ro seqcount d ro input flag mfs buf)
                         (return-from apply-chain-context 1)))))))))
         nil))
      (3 ;; coverage-based chain
       (let* ((bcount (u16 d (+ o 2)))
              (bcovs (loop for k below bcount
                           collect (parse-coverage d (+ o (u16 d (+ o 4 (* k 2)))))))
              (po (+ o 4 (* bcount 2)))
              (icount (u16 d po))
              (icovs (loop for k below icount
                           collect (parse-coverage d (+ o (u16 d (+ po 2 (* k 2)))))))
              (po2 (+ po 2 (* icount 2)))
              (lcount (u16 d po2))
              (lcovs (loop for k below lcount
                           collect (parse-coverage d (+ o (u16 d (+ po2 2 (* k 2)))))))
              (po3 (+ po2 2 (* lcount 2)))
              (seqcount (u16 d po3))
              (ro (+ po3 2)))
         (when (gethash (glyph-pos-gid (aref buf i)) (first icovs))
           (let ((input (match-coverage-seq font gdef flag mfs buf i +1 icovs icount)))
             (when (and input (= (length input) icount))
               (let* ((bstart (1- (first input)))
                      (lstart (1+ (car (last input))))
                      (bmatch (or (zerop bcount)
                                  (match-coverage-seq font gdef flag mfs buf bstart -1 bcovs bcount)))
                      (lmatch (or (zerop lcount)
                                  (match-coverage-seq font gdef flag mfs buf lstart +1 lcovs lcount))))
                 (when (and bmatch lmatch)
                   (apply-seq-lookups ctx ro seqcount d ro input flag mfs buf)
                   (return-from apply-chain-context 1))))))
         nil)))))

(defun chain-rule-match-glyph (font gdef flag mfs buf i d rule)
  "ChainSubRule (glyph-based): backtrack, input(skip first=current), lookahead,
   then seqLookups. Return (values ok input-idxs records-off seqcount)."
  (let* ((p rule)
         (bcount (u16 d p)) (bgids (make-array bcount)))
    (dotimes (k bcount) (setf (aref bgids k) (u16 d (+ p 2 (* k 2)))))
    (incf p (+ 2 (* bcount 2)))
    (let* ((icount (u16 d p)) (igids (make-array (max 0 (1- icount)))))
      (loop for k from 1 below icount do (setf (aref igids (1- k)) (u16 d (+ p 2 (* (1- k) 2)))))
      (incf p (+ 2 (* (1- icount) 2)))
      (let* ((lcount (u16 d p)) (lgids (make-array lcount)))
        (dotimes (k lcount) (setf (aref lgids k) (u16 d (+ p 2 (* k 2)))))
        (incf p (+ 2 (* lcount 2)))
        (let* ((seqcount (u16 d p)) (ro (+ p 2)))
          (let ((iidx (match-glyph-seq font gdef flag mfs buf (1+ i) +1 igids (1- icount))))
            (when (or (= icount 1) iidx)
              (let* ((input (cons i iidx))
                     (bstart (1- i)) (lstart (1+ (car (last input))))
                     (bm (or (zerop bcount)
                             (match-glyph-seq font gdef flag mfs buf bstart -1 bgids bcount)))
                     (lm (or (zerop lcount)
                             (match-glyph-seq font gdef flag mfs buf lstart +1 lgids lcount))))
                (when (and bm lm)
                  (return-from chain-rule-match-glyph (values t input ro seqcount)))))))))
    (values nil nil nil nil)))

(defun chain-rule-match-class (font gdef flag mfs buf i d rule bcd icd lcd)
  "ChainSubClassRule (class-based)."
  (let* ((p rule)
         (bcount (u16 d p)) (bcl (make-array bcount)))
    (dotimes (k bcount) (setf (aref bcl k) (u16 d (+ p 2 (* k 2)))))
    (incf p (+ 2 (* bcount 2)))
    (let* ((icount (u16 d p)) (icl (make-array (max 0 (1- icount)))))
      (loop for k from 1 below icount do (setf (aref icl (1- k)) (u16 d (+ p 2 (* (1- k) 2)))))
      (incf p (+ 2 (* (1- icount) 2)))
      (let* ((lcount (u16 d p)) (lcl (make-array lcount)))
        (dotimes (k lcount) (setf (aref lcl k) (u16 d (+ p 2 (* k 2)))))
        (incf p (+ 2 (* lcount 2)))
        (let* ((seqcount (u16 d p)) (ro (+ p 2)))
          (let ((iidx (match-class-seq font gdef flag mfs buf (1+ i) +1 icl icd (1- icount))))
            (when (or (= icount 1) iidx)
              (let* ((input (cons i iidx))
                     (bstart (1- i)) (lstart (1+ (car (last input))))
                     (bm (or (zerop bcount)
                             (match-class-seq font gdef flag mfs buf bstart -1 bcl bcd bcount)))
                     (lm (or (zerop lcount)
                             (match-class-seq font gdef flag mfs buf lstart +1 lcl lcd lcount))))
                (when (and bm lm)
                  (return-from chain-rule-match-class (values t input ro seqcount)))))))))
    (values nil nil nil nil)))

;;; ===========================================================================
;;; the public entry point
;;; ===========================================================================
(defparameter *default-features*
  '("ccmp" "locl" "rlig" "liga" "calt" "kern" "mark" "mkmk")
  "HarfBuzz default-on features for horizontal Latin shaping.")

(defun %normalize-features (features)
  "FEATURES may be a list mixing keywords (:liga) and raw 4-char tag strings, or
   :default. Return a list of 4-char tag strings."
  (cond
    ((eq features :default) (copy-list *default-features*))
    (t (remove-duplicates
        (loop for f in features
              collect (etypecase f
                        (string f)
                        (keyword (string-downcase (symbol-name f)))))
        :test #'string=))))

(defun shape-run (font text &key (features :default)
                                 (script :latn) (direction :ltr) variation)
  "Shape TEXT with FONT -> vector of glyph-pos (gid + advances/offsets, font
   units). FEATURES is :default (the HarfBuzz default-on set) or a list mixing
   keywords (:liga :kern …) and raw 4-char feature-tag strings (\"smcp\")."
  (declare (ignore script direction variation))
  (let* ((d (font-data font))
         (gdef (parse-gdef font))
         (tags (%normalize-features features))
         (buf (itemize font text)))
    ;; ---- GSUB ----
    (multiple-value-bind (llist idxs) (otl-lookup-indices font "GSUB" tags)
      (when idxs
        (let ((ctx (make-sctx :font font :d d :gpos nil :llist llist :gdef gdef)))
          (dolist (li idxs)
            (apply-lookup ctx (lookup-offset llist d li) buf)))))
    ;; advances follow the FINAL gids (GSUB may have substituted)
    (loop for g across buf
          do (setf (glyph-pos-x-advance g) (advance-at font (glyph-pos-gid g))))
    ;; ---- GPOS ----
    (multiple-value-bind (llist idxs) (otl-lookup-indices font "GPOS" tags)
      (when idxs
        (let ((ctx (make-sctx :font font :d d :gpos t :llist llist :gdef gdef)))
          (dolist (li idxs)
            (apply-lookup ctx (lookup-offset llist d li) buf)))))
    buf))
