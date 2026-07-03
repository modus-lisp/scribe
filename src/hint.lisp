;;;; hint.lisp — TrueType bytecode hinting (the font's own instructions).
;;;;
;;;; Grid-fits a glyph's points at a target ppem so small text is crisp, matching
;;;; FreeType's classic interpreter (v35, full x+y grid-fit).  All arithmetic is
;;;; F26Dot6 (px*64) for coordinates/distances and F2Dot14 for unit vectors, to
;;;; match FreeType bit-for-bit (see docs/HINTING.md; oracle: inspect/hint-oracle.py).
;;;;
;;;; Pipeline: parse cvt/fpgm/prep + per-glyph instructions -> run fpgm once,
;;;; prep per-ppem, then the glyph program -> the moved points are the hinted
;;;; outline.  Points live in ZONES; a glyph's real points are zone 1 plus 4
;;;; phantom points (lsb, adv, top, bottom); zone 0 is the twilight zone.
(in-package #:scribe)

;;; ------------------------------------------------------------------ maxp v1
(defun parse-maxp1 (font)
  "maxp v1 interpreter limits (or NILs for v0.5 fonts with no glyf hints)."
  (let* ((d (font-data font)) (o (req-table font "maxp")))
    (when (>= (u32 d o) #x00010000)
      (list :max-points (u16 d (+ o 6)) :max-contours (u16 d (+ o 8))
            :max-twilight (u16 d (+ o 16)) :max-storage (u16 d (+ o 18))
            :max-functions (u16 d (+ o 20)) :max-instruction-defs (u16 d (+ o 22))
            :max-stack (u16 d (+ o 24))))))

(defun table-bytes (font tag)
  "Raw bytes of TAG as a subsequence of the font data, or NIL if absent."
  (multiple-value-bind (off len) (font-table font tag)
    (when off (subseq (font-data font) off (+ off len)))))

(defun parse-cvt (font)
  "The Control Value Table as a vector of s16 FUnits (empty if absent)."
  (let ((b (table-bytes font "cvt ")))
    (if (null b) (make-array 0 :element-type 'fixnum)
        (let* ((n (floor (length b) 2)) (v (make-array n :element-type 'fixnum)))
          (dotimes (i n v)
            (let ((x (logior (ash (aref b (* i 2)) 8) (aref b (1+ (* i 2))))))
              (setf (aref v i) (if (>= x #x8000) (- x #x10000) x))))))))

(defun hmtx-metrics (font gid)
  "(values advance-width lsb) in FUnits for GID (last advance repeats past nh)."
  (let* ((d (font-data font)) (off (req-table font "hmtx")) (nh (font-num-h-metrics font)))
    (if (< gid nh)
        (values (u16 d (+ off (* gid 4))) (s16 d (+ off (* gid 4) 2)))
        (values (u16 d (+ off (* (1- nh) 4))) (s16 d (+ off (* nh 4) (* (- gid nh) 2)))))))

;;; ---- construction ----
(defun hscale (h funit)
  "Scale a FUnit value to F26.6 pixels at this hinter's ppem."
  (muldiv funit (* 64 (hs-ppem h)) (font-units-per-em (hs-font h))))

(defun make-hinter (font ppem)
  "A hinter ready to run FONT's programs at PPEM (stack/storage from maxp, CVT
scaled to F26.6).  Call RUN-FPGM-PREP before hinting glyphs."
  (let* ((mx (parse-maxp1 font)) (upem (font-units-per-em font))
         (raw (parse-cvt font)) (n (length raw))
         (h (make-hint :font font :ppem ppem :scale (* 64 ppem)
                       :stack (make-array (max 256 (+ 64 (or (getf mx :max-stack) 256))) :element-type 'fixnum)
                       :storage (make-array (max 16 (1+ (or (getf mx :max-storage) 16))) :element-type 'fixnum :initial-element 0)
                       :cvt (make-array n :element-type 'fixnum))))
    (dotimes (i n) (setf (aref (hs-cvt h) i) (muldiv (aref raw i) (* 64 ppem) upem)))
    h))

;;; The graphics-state slots that make up the interpreter's GS.  FreeType restores
;;; the whole post-prep GS before every glyph, so a glyph program's changes (round
;;; mode, SMD, SDB, vectors, ...) never leak into the next glyph.
(defparameter *gs-slots*
  '(hs-fx hs-fy hs-px hs-py hs-dpx hs-dpy hs-rp0 hs-rp1 hs-rp2 hs-zp0 hs-zp1 hs-zp2
    hs-loop hs-round-state hs-round-period hs-round-phase hs-round-threshold
    hs-min-dist hs-cvt-cut-in hs-sw-cut-in hs-sw-value hs-auto-flip
    hs-delta-base hs-delta-shift hs-scan-control hs-instruct-control))

(defun snapshot-gs (h)
  (setf (hs-gs-default h) (mapcar (lambda (s) (funcall s h)) *gs-slots*)))
(defun restore-gs (h)
  (loop for s in *gs-slots* for v in (hs-gs-default h)
        do (funcall (fdefinition `(setf ,s)) v h)))

(defun run-fpgm-prep (h)
  "Run the font program (defines functions) then the control-value program (sets
graphics state / CVT for this ppem), then snapshot the GS as the per-glyph default."
  (let ((fp (table-bytes (hs-font h) "fpgm"))) (when fp (run-program h fp)))
  (let ((pp (table-bytes (hs-font h) "prep"))) (when pp (run-program h pp)))
  (snapshot-gs h))

;;; ------------------------------------------------------------- fixed point
;;; F26Dot6: px*64.  F2Dot14: unit vector components (16384 = 1.0).
(declaim (inline f26.6 muldiv))
(defun f26.6 (px) (round (* px 64)))
(defun muldiv (a b c)
  "round(a*b/c) with .5 rounding away from zero — FreeType's FT_MulDiv.  The
result sign is sign(a*b*c): dividing by a negative c flips it."
  (if (zerop c) 0
      (let* ((ab (* a b)) (s (if (minusp (* ab c)) -1 1))
             (n (+ (abs ab) (floor (abs c) 2))))
        (* s (floor n (abs c))))))
(declaim (inline mul2.14 dot14))
(defun mul2.14 (a b) (muldiv a b 16384))           ; F26.6 * F2.14 -> F26.6
(defun dot14 (ax ay bx by)
  "Dot product (ax,ay)·(bx,by) with the second vector in F2.14, rounded ONCE after
summing — exactly FreeType's TT_DotFix14.  (Rounding each term separately, as two
mul2.14 calls, can differ by 1 and cascades into 1-unit mover errors.)"
  (let ((s (+ (* ax bx) (* ay by))))
    (ash (+ s #x2000 (if (minusp s) -1 0)) -14)))

;;; ------------------------------------------------------------- interpreter
(defstruct hz                                     ; a zone of points
  (cur-x nil) (cur-y nil)                          ; current coords, F26.6 arrays
  (org-x nil) (org-y nil)                          ; original (scaled) coords
  (orus-x nil) (orus-y nil)                        ; original UNSCALED coords (font units)
  (on nil)                                         ; on-curve flags
  (touch nil))                                     ; touch flags (bit0 x, bit1 y)

(defstruct (hint (:conc-name hs-))
  font ppem scale                                  ; scale = F26.6-per-FUnit (muldiv-able)
  (stack (make-array 256 :element-type 'fixnum)) (sp 0)
  storage cvt                                       ; storage area + scaled CVT (F26.6)
  (functions (make-hash-table))                     ; fn# -> (code . pc)
  code (pc 0)                                        ; current program
  ;; graphics state (F26.6 / F2.14)
  (fx 16384) (fy 0) (px 16384) (py 0) (dpx 16384) (dpy 0)  ; freedom, proj, dual-proj
  (rp0 0) (rp1 0) (rp2 0) (zp0 1) (zp1 1) (zp2 1) (loop 1)
  (round-state :grid) (min-dist 64) (cvt-cut-in 68) (sw-cut-in 0) (sw-value 0)
  (auto-flip t) (delta-base 9) (delta-shift 3) (scan-control 0) (instruct-control 0)
  (round-period 64) (round-phase 0) (round-threshold 32)
  zones                                             ; #(twilight glyph)
  (glyph-ends nil) (glyph-npts 0)                   ; contour end indices + #real points
  (gs-default nil))                                 ; graphics state snapshot after prep

(define-condition hint-unimplemented (error)
  ((op :initarg :op :reader hint-op))
  (:report (lambda (c s) (format s "hint: unimplemented opcode #x~2,'0X" (hint-op c)))))

;;; ---- stack helpers ----
(declaim (inline hpush hpop))
(defun hpush (h v) (setf (aref (hs-stack h) (hs-sp h)) v) (incf (hs-sp h)))
(defun hpop (h) (decf (hs-sp h)) (aref (hs-stack h) (hs-sp h)))

;;; ---- rounding engine (round-state -> round an F26.6 distance) ----
(defun hround (h dist)
  "Round F26.6 DIST per the graphics-state round mode (FT_Round_Super), preserving
sign.  A negative result clamps to 0.  (The 'don't collapse a nonzero distance'
rule is min-distance, applied by MDRP/MIRP — not here.)"
  (if (eq (hs-round-state h) :off) dist
      (let* ((per (hs-round-period h)) (ph (hs-round-phase h)) (thr (hs-round-threshold h))
             (sign (if (minusp dist) -1 1)) (d (abs dist))
             (r (+ (* (floor (+ (- d ph) thr) per) per) ph)))
        (when (minusp r) (setf r 0))
        (* sign r))))

(defun set-round-period (h period phase threshold)
  (setf (hs-round-period h) period (hs-round-phase h) phase (hs-round-threshold h) threshold))

;;; ---- projection / movement ----
(defun hproject (h x y)
  "Projection of vector (x,y) F26.6 onto the projection vector -> F26.6."
  (dot14 x y (hs-px h) (hs-py h)))
(defun hdual-project (h x y) (dot14 x y (hs-dpx h) (hs-dpy h)))

(defun point-xy (z i) (values (aref (hz-cur-x z) i) (aref (hz-cur-y z) i)))
(defun zone (h zp) (aref (hs-zones h) zp))

(defun move-point (h z i dist)
  "Move point I of zone Z by DIST (F26.6) along the freedom vector; mark touched.
F_dot_P (freedom·projection) is clamped to 0x4000 when |F_dot_P| < 0x400, exactly as
FreeType's Compute_Funcs — otherwise a near-perpendicular freedom/projection would
divide by ~0 and drop (or spike) the move (e.g. DejaVu '$' diagonal at small ppem)."
  (let* ((fx (hs-fx h)) (fy (hs-fy h))
         (fdotp (dot14 fx fy (hs-px h) (hs-py h))))
    (when (< (abs fdotp) #x400) (setf fdotp #x4000))
    (when (/= fx 0) (incf (aref (hz-cur-x z) i) (muldiv dist fx fdotp))
          (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 1)))
    (when (/= fy 0) (incf (aref (hz-cur-y z) i) (muldiv dist fy fdotp))
          (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 2)))))

;;; ---- program bytes ----
(declaim (inline nextb))
(defun nextb (h) (prog1 (aref (hs-code h) (hs-pc h)) (incf (hs-pc h))))
(defun nextw (h) (let ((hi (nextb h)) (lo (nextb h)))
                   (let ((x (logior (ash hi 8) lo))) (if (>= x #x8000) (- x #x10000) x))))

;;; ---- the eval loop ----
(defvar *hint-trace* (and (uiop:getenv "WEFT_HINT_TRACE") t)
  "When set (env WEFT_HINT_TRACE), dump per-instruction interpreter state to match
FreeType's trace build (ftbuild) for line-by-line execution diffs.")

(defun hint-trace-state (h)
  "Emit one STATE line mirroring the patched FreeType ttinterp trace format."
  (let ((z (ignore-errors (zone h 1))))
    (format *error-output* "STATE pc=~a op=0x~2,'0X proj=(~a,~a) free=(~a,~a) rp=~a,~a,~a zp=~a,~a,~a loop=~a |"
            (hs-pc h) (aref (hs-code h) (hs-pc h)) (hs-px h) (hs-py h) (hs-fx h) (hs-fy h)
            (hs-rp0 h) (hs-rp1 h) (hs-rp2 h) (hs-zp0 h) (hs-zp1 h) (hs-zp2 h) (hs-loop h))
    (when z
      (dotimes (i (length (hz-cur-x z)))
        (format *error-output* " ~a:(~a,~a)~a" i (aref (hz-cur-x z) i) (aref (hz-cur-y z) i)
                (if (plusp (aref (hz-touch z) i)) "*" " "))))
    (format *error-output* "~%")))

(defun run-program (h code)
  "Execute CODE (a byte vector) to completion (pc past end)."
  (setf (hs-code h) code (hs-pc h) 0)
  (loop while (< (hs-pc h) (length code)) do
    (when *hint-trace* (hint-trace-state h))
    (step-op h)))

(defun step-op (h)
  (let ((op (nextb h)))
    (cond
      ;; PUSHB[abc] / PUSHW[abc] / NPUSHB / NPUSHW
      ((<= #xB0 op #xB7) (dotimes (i (+ 1 (- op #xB0))) (hpush h (nextb h))))
      ((<= #xB8 op #xBF) (dotimes (i (+ 1 (- op #xB8))) (hpush h (nextw h))))
      ((= op #x40) (dotimes (i (nextb h)) (hpush h (nextb h))))          ; NPUSHB
      ((= op #x41) (dotimes (i (nextb h)) (hpush h (nextw h))))          ; NPUSHW
      (t (funcall (op-fn op) h op)))))

;;; opcode table: filled by DEF-OP; unknown -> the trap so coverage is data-driven
(defvar *ops* (make-array 256 :initial-element nil))
(defun op-fn (op) (or (aref *ops* op) (lambda (h o) (declare (ignore h)) (error 'hint-unimplemented :op o))))
(defmacro def-op (code (h &optional (opv (gensym))) &body body)
  `(setf (aref *ops* ,code) (lambda (,h ,opv) (declare (ignorable ,h ,opv)) ,@body)))

;;; ---- stack / arithmetic / logic ----
(def-op #x20 (h) (let ((v (hpop h))) (hpush h v) (hpush h v)))            ; DUP
(def-op #x21 (h) (hpop h))                                                ; POP
(def-op #x22 (h) (setf (hs-sp h) 0))                                      ; CLEAR
(def-op #x23 (h) (let ((a (hpop h)) (b (hpop h))) (hpush h a) (hpush h b))) ; SWAP
(def-op #x24 (h) (hpush h (hs-sp h)))                                     ; DEPTH
(def-op #x60 (h) (hpush h (+ (hpop h) (hpop h))))                         ; ADD
(def-op #x61 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (- a b))))     ; SUB
(def-op #x62 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (zerop b) 0 (muldiv a 64 b))))) ; DIV: (a*64)/b
(def-op #x63 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (muldiv a b 64)))) ; MUL: (a*b)/64
(def-op #x64 (h) (hpush h (abs (hpop h))))                               ; ABS
(def-op #x65 (h) (hpush h (- (hpop h))))                                 ; NEG
(def-op #x66 (h) (hpush h (* 64 (floor (hpop h) 64))))                   ; FLOOR
(def-op #x67 (h) (hpush h (* 64 (ceiling (hpop h) 64))))                 ; CEILING
(def-op #x68 (h) (hpush h (hround h (hpop h))))                        ; ROUND[00]
(def-op #x69 (h) (hpush h (hround h (hpop h))))                        ; ROUND[01]
(def-op #x6A (h) (hpush h (hround h (hpop h))))                        ; ROUND[10]
(def-op #x6B (h) (hpush h (hround h (hpop h))))                        ; ROUND[11]
;; NROUND[abcd]: grayscale engine compensation is 0 -> identity (value stays)
(def-op #x6C (h) nil) (def-op #x6D (h) nil) (def-op #x6E (h) nil) (def-op #x6F (h) nil)
(def-op #x50 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (< a b) 1 0))))  ; LT
(def-op #x51 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (<= a b) 1 0)))) ; LTEQ
(def-op #x52 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (> a b) 1 0))))  ; GT
(def-op #x53 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (>= a b) 1 0)))) ; GTEQ
(def-op #x54 (h) (hpush h (if (= (hpop h) (hpop h)) 1 0)))               ; EQ
(def-op #x55 (h) (hpush h (if (/= (hpop h) (hpop h)) 1 0)))              ; NEQ
(def-op #x56 (h) (hpush h (if (logbitp 6 (hround h (hpop h))) 1 0)))     ; ODD
(def-op #x57 (h) (hpush h (if (logbitp 6 (hround h (hpop h))) 0 1)))     ; EVEN
(def-op #x5A (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (and (/= a 0) (/= b 0)) 1 0)))) ; AND
(def-op #x5B (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (if (or (/= a 0) (/= b 0)) 1 0))))  ; OR
(def-op #x5C (h) (hpush h (if (zerop (hpop h)) 1 0)))                    ; NOT
(def-op #x8A (h) (let ((a (hpop h)) (b (hpop h)) (c (hpop h))) (hpush h b) (hpush h a) (hpush h c))) ; ROLL
(def-op #x8B (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (max a b))))  ; MAX
(def-op #x8C (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (min a b))))  ; MIN

;;; ---- storage / cvt / measurement ----
(def-op #x43 (h) (hpush h (aref (hs-storage h) (hpop h))))               ; RS
(def-op #x42 (h) (let ((v (hpop h)) (i (hpop h))) (setf (aref (hs-storage h) i) v))) ; WS
(def-op #x44 (h) (let ((v (hpop h)) (i (hpop h))) (setf (aref (hs-cvt h) i) v)))     ; WCVTP
(def-op #x70 (h) (let ((v (hpop h)) (i (hpop h)))                        ; WCVTF (value in FUnits)
                   (setf (aref (hs-cvt h) i) (hscale h v))))
(def-op #x45 (h) (hpush h (aref (hs-cvt h) (hpop h))))                   ; RCVT
(def-op #x4B (h) (hpush h (hs-ppem h)))                                  ; MPPEM (integer ppem)
(def-op #x4C (h) (hpush h (hs-ppem h)))                                  ; MPS (point size ~= ppem here)
(def-op #x88 (h)                                                        ; GETINFO
  ;; report FreeType classic interpreter (v35), grayscale, not rotated/stretched.
  (let ((sel (hpop h)) (r 0))
    (when (logbitp 0 sel) (setf r (logior r 35)))                       ; rasterizer version
    (when (logbitp 5 sel) (setf r (logior r #x1000)))                   ; grayscale/smooth
    (hpush h r)))

;;; ---- DELTA exceptions (per-ppem tweaks to CVT entries / points) ----
;;; Stack (top-down after n): argN, ptN, ...  Each arg = (relppem<<4)|mag; the
;;; exception fires only at ppem = base + relppem.  mag bits 0..15 -> steps
;;; -8..-1,+1..+8; step = 1/(2^delta-shift) px = (64 >> delta-shift) in F26.6.
(defun delta-step (h arg)
  (let ((bits (logand arg #xF)))
    (* (if (< bits 8) (- bits 8) (- bits 7)) (ash 64 (- (hs-delta-shift h))))))
;;; Stack (top-down after n): the cvt/point INDEX is nearer the top, the exception
;;; ARG is below it (spec: "... arg1 idx1 n").  So pop index first, then arg.
(defun do-deltac (h base-off)
  (let ((n (hpop h)) (base (+ (hs-delta-base h) base-off)))
    (dotimes (i n)
      (let ((idx (hpop h)) (arg (hpop h)))
        (when (= (hs-ppem h) (+ base (ash arg -4)))
          (incf (aref (hs-cvt h) idx) (delta-step h arg)))))))
(defun do-deltap (h base-off)
  (let ((n (hpop h)) (base (+ (hs-delta-base h) base-off)))
    (dotimes (i n)
      (let ((pt (hpop h)) (arg (hpop h)))
        (when (= (hs-ppem h) (+ base (ash arg -4)))
          (move-point h (zone h (hs-zp0 h)) pt (delta-step h arg)))))))
(def-op #x73 (h) (do-deltac h 0))                                       ; DELTAC1
(def-op #x74 (h) (do-deltac h 16))                                      ; DELTAC2
(def-op #x75 (h) (do-deltac h 32))                                      ; DELTAC3
(def-op #x5D (h) (do-deltap h 0))                                       ; DELTAP1
(def-op #x71 (h) (do-deltap h 16))                                      ; DELTAP2
(def-op #x72 (h) (do-deltap h 32))                                      ; DELTAP3

;;; ---- graphics-state setters ----
(def-op #x00 (h) (setf (hs-px h) 0 (hs-py h) 16384 (hs-dpx h) 0 (hs-dpy h) 16384
                       (hs-fx h) 0 (hs-fy h) 16384))                     ; SVTCA[y]
(def-op #x01 (h) (setf (hs-px h) 16384 (hs-py h) 0 (hs-dpx h) 16384 (hs-dpy h) 0
                       (hs-fx h) 16384 (hs-fy h) 0))                     ; SVTCA[x]
;; opcode&1 selects axis: 1=x, 0=y (so 0x02/0x04 are the Y variants).
(def-op #x02 (h) (setf (hs-px h) 0 (hs-py h) 16384 (hs-dpx h) 0 (hs-dpy h) 16384)) ; SPVTCA[y]
(def-op #x03 (h) (setf (hs-px h) 16384 (hs-py h) 0 (hs-dpx h) 16384 (hs-dpy h) 0)) ; SPVTCA[x]
(def-op #x04 (h) (setf (hs-fx h) 0 (hs-fy h) 16384))                     ; SFVTCA[y]
(def-op #x05 (h) (setf (hs-fx h) 16384 (hs-fy h) 0))                     ; SFVTCA[x]
(def-op #x0E (h) (setf (hs-fx h) (hs-px h) (hs-fy h) (hs-py h)))         ; SFVTPV
(def-op #x10 (h) (setf (hs-rp0 h) (hpop h)))                             ; SRP0
(def-op #x11 (h) (setf (hs-rp1 h) (hpop h)))                             ; SRP1
(def-op #x12 (h) (setf (hs-rp2 h) (hpop h)))                             ; SRP2
(def-op #x13 (h) (setf (hs-zp0 h) (hpop h)))                             ; SZP0
(def-op #x14 (h) (setf (hs-zp1 h) (hpop h)))                             ; SZP1
(def-op #x15 (h) (setf (hs-zp2 h) (hpop h)))                             ; SZP2
(def-op #x16 (h) (let ((v (hpop h))) (setf (hs-zp0 h) v (hs-zp1 h) v (hs-zp2 h) v))) ; SZPS
(def-op #x17 (h) (setf (hs-loop h) (hpop h)))                            ; SLOOP
(def-op #x1A (h) (setf (hs-min-dist h) (hpop h)))                        ; SMD
(def-op #x1D (h) (setf (hs-cvt-cut-in h) (hpop h)))                      ; SCVTCI
(def-op #x1E (h) (setf (hs-sw-cut-in h) (hpop h)))                       ; SSWCI
(def-op #x1F (h) (setf (hs-sw-value h) (hpop h)))                        ; SSW (FUnits->F26.6 approx)
(def-op #x4D (h) (setf (hs-auto-flip h) t))                             ; FLIPON
(def-op #x4E (h) (setf (hs-auto-flip h) nil))                           ; FLIPOFF
(def-op #x5E (h) (setf (hs-delta-base h) (hpop h)))                     ; SDB
(def-op #x5F (h) (setf (hs-delta-shift h) (hpop h)))                    ; SDS
(def-op #x85 (h) (setf (hs-scan-control h) (hpop h)))                   ; SCANCTRL
(def-op #x8D (h) (setf (hs-scan-control h) (hpop h)))                   ; SCANTYPE (fold: consume)
(def-op #x8E (h) (setf (hs-instruct-control h) (progn (hpop h) (hpop h) 0))) ; INSTCTRL (consume 2)

;;; ---- rounding-state setters (correct opcodes; RTDG=0x3D, ROFF=0x7A) ----
(def-op #x18 (h) (setf (hs-round-state h) :grid)   (set-round-period h 64 0 32))  ; RTG
(def-op #x19 (h) (setf (hs-round-state h) :grid)   (set-round-period h 64 32 32)) ; RTHG (phase=half)
(def-op #x3D (h) (setf (hs-round-state h) :grid)   (set-round-period h 32 0 16))  ; RTDG
(def-op #x7A (h) (setf (hs-round-state h) :off))                                  ; ROFF
(def-op #x7C (h) (setf (hs-round-state h) :grid)   (set-round-period h 64 0 63))  ; RUTG (up=ceil: thr=period-1)
(def-op #x7D (h) (setf (hs-round-state h) :grid)   (set-round-period h 64 0 0))   ; RDTG (down)

;;; ---- flow control: IF/ELSE/EIF, function defs, calls, jumps ----
(defun skip-to (h &rest ends)
  "Advance pc past a matching EIF/ELSE, honoring nested IFs.  ENDS = opcodes that
stop the skip at depth 0 (0x59 EIF, and optionally 0x1B ELSE)."
  (let ((depth 0))
    (loop
      (let ((op (nextb h)))
        (cond ((= op #x58) (incf depth))                                 ; nested IF
              ((and (= op #x59) (zerop depth)) (return))                 ; EIF
              ((= op #x59) (decf depth))
              ((and (member op ends) (zerop depth)) (return))
              ;; skip inline push data so we don't misread it as opcodes
              ((<= #xB0 op #xB7) (dotimes (i (+ 1 (- op #xB0))) (nextb h)))
              ((<= #xB8 op #xBF) (dotimes (i (+ 1 (- op #xB8))) (nextb h) (nextb h)))
              ((= op #x40) (dotimes (i (nextb h)) (nextb h)))
              ((= op #x41) (dotimes (i (nextb h)) (nextb h) (nextb h))))))))
(def-op #x58 (h) (if (zerop (hpop h)) (skip-to h #x1B) nil))            ; IF (false -> skip to ELSE/EIF)
(def-op #x1B (h) (skip-to h))                                          ; ELSE (THEN fell here -> skip to EIF)
(def-op #x59 (h) nil)                                                  ; EIF
(def-op #x1C (h) (let ((off (hpop h))) (setf (hs-pc h) (+ (- (hs-pc h) 1) off)))) ; JMPR
(def-op #x78 (h) (let ((e (hpop h)) (off (hpop h)))                    ; JROT (jump if true)
                   (unless (zerop e) (setf (hs-pc h) (+ (- (hs-pc h) 1) off)))))
(def-op #x79 (h) (let ((e (hpop h)) (off (hpop h)))                    ; JROF (jump if false)
                   (when (zerop e) (setf (hs-pc h) (+ (- (hs-pc h) 1) off)))))

(def-op #x2C (h)                                                        ; FDEF
  (let ((fn (hpop h)) (start (hs-pc h)))
    (setf (gethash fn (hs-functions h)) (cons (hs-code h) start))
    ;; skip body to ENDF (0x2D)
    (loop for op = (nextb h) until (= op #x2D) do
      (cond ((<= #xB0 op #xB7) (dotimes (i (+ 1 (- op #xB0))) (nextb h)))
            ((<= #xB8 op #xBF) (dotimes (i (+ 1 (- op #xB8))) (nextb h) (nextb h)))
            ((= op #x40) (dotimes (i (nextb h)) (nextb h)))
            ((= op #x41) (dotimes (i (nextb h)) (nextb h) (nextb h)))))))
(def-op #x2D (h) nil)                                                   ; ENDF (only hit at top level)

(defun call-fn (h fn)
  (let ((def (gethash fn (hs-functions h))))
    (unless def (error "hint: CALL to undefined function ~a" fn))
    (let ((save-code (hs-code h)) (save-pc (hs-pc h)))
      (setf (hs-code h) (car def) (hs-pc h) (cdr def))
      (loop for op = (aref (hs-code h) (hs-pc h))
            do (when (= op #x2D) (return))                              ; ENDF
               (when *hint-trace* (hint-trace-state h))                 ; trace into CALLs (align w/ FreeType)
               (step-op h))
      (setf (hs-code h) save-code (hs-pc h) save-pc))))
(def-op #x2B (h) (call-fn h (hpop h)))                                 ; CALL
(def-op #x2A (h) (let ((fn (hpop h)) (n (hpop h))) (dotimes (i n) (call-fn h fn)))) ; LOOPCALL

;;; ------------------------------------------------------- glyph loading
(defun glyph-instructions (font gid)
  "The per-glyph instruction byte stream (or NIL), for a simple or composite glyph."
  (multiple-value-bind (off len) (loca-offset font gid)
    (when (plusp len)
      (let* ((d (font-data font)) (g (+ (req-table font "glyf") off)) (ncont (s16 d g)))
        (if (>= ncont 0)
            (let* ((p (+ g 10 (* 2 ncont))) (ilen (u16 d p)))
              (when (plusp ilen) (subseq d (+ p 2) (+ p 2 ilen))))
            ;; composite: instructions follow the components iff any component has
            ;; WE_HAVE_INSTRUCTIONS (0x100).  parse-composite-components returns the
            ;; byte offset just past the last component.
            (multiple-value-bind (comps p) (parse-composite-components d g)
              (when (some (lambda (comp) (logbitp 8 (second comp))) comps)
                (let ((ilen (u16 d p)))
                  (when (plusp ilen) (subseq d (+ p 2) (+ p 2 ilen)))))))))))

;;; ---- composite glyphs: components + (optional) composite instructions ----
(defun parse-composite-components (d g)
  "Parse a composite glyph header at byte offset G into a list of components, each
(cgid flags dx-or-p1 dy-or-p2 a b c e) — 2x2 transform in F2.14, args in FUnits."
  (let ((p (+ g 10)) (out '()) (more t))
    (loop while more do
      (let* ((flags (u16 d p)) (cgid (u16 d (+ p 2))) (dx 0) (dy 0)
             (a 16384) (b 0) (c 0) (e 16384))
        (incf p 4)
        (if (logbitp 0 flags)                              ; ARG_1_AND_2_ARE_WORDS
            (progn (setf dx (s16 d p) dy (s16 d (+ p 2))) (incf p 4))
            (progn (setf dx (let ((v (u8 d p))) (if (>= v 128) (- v 256) v))
                         dy (let ((v (u8 d (1+ p)))) (if (>= v 128) (- v 256) v))) (incf p 2)))
        (flet ((f214 (o) (let ((v (u16 d o))) (if (>= v #x8000) (- v #x10000) v))))
          (cond ((logbitp 3 flags) (setf a (f214 p) e a) (incf p 2))                 ; WE_HAVE_A_SCALE
                ((logbitp 6 flags) (setf a (f214 p) e (f214 (+ p 2))) (incf p 4))     ; X_AND_Y_SCALE
                ((logbitp 7 flags) (setf a (f214 p) b (f214 (+ p 2))                  ; TWO_BY_TWO
                                         c (f214 (+ p 4)) e (f214 (+ p 6))) (incf p 8))))
        (push (list cgid flags dx dy a b c e) out)
        (setf more (logbitp 5 flags))))                    ; MORE_COMPONENTS
    (values (nreverse out) p)))                            ; P now points past components

(defun glyph-points (h gid)
  "Merged points for a simple OR composite GID: (values ux uy cx cy on endpts).
UX/UY are original FUnit coords, CX/CY scaled F26.6 (component xy-offsets grid-fit
per ROUND_XY_TO_GRID); components recurse + carry the 2x2 transform."
  (let* ((font (hs-font h)) (d (font-data font)))
    (multiple-value-bind (off len) (loca-offset font gid)
      (if (zerop len) (values #() #() #() #() #() #())
          (let* ((g (+ (req-table font "glyf") off)) (ncont (s16 d g)))
            (if (>= ncont 0)
                (multiple-value-bind (xs ys flags endpts) (%simple-glyph d g ncont)
                  (let* ((np (length xs)) (cx (make-array np)) (cy (make-array np)) (on (make-array np)))
                    (dotimes (i np)
                      (setf (aref cx i) (hscale h (aref xs i)) (aref cy i) (hscale h (aref ys i))
                            (aref on i) (if (logbitp 0 (aref flags i)) 1 0)))
                    (values xs ys cx cy on endpts)))
                (let ((uxs '()) (uys '()) (cxs '()) (cys '()) (ons '()) (ends '()) (base 0))
                  (dolist (comp (parse-composite-components d g))
                    (destructuring-bind (cgid flags dx dy a b c e) comp
                      (let ((rxy (logbitp 2 flags)) (xy (logbitp 1 flags)))
                        ;; component is HINTED standalone, then transformed + placed
                        (multiple-value-bind (ccx ccy cux cuy con cend) (hint-subglyph h cgid)
                          (let* ((n (length ccx))
                                 (ox (if xy (let ((s (hscale h dx))) (if rxy (* 64 (floor (+ s 32) 64)) s)) 0))
                                 (oy (if xy (let ((s (hscale h dy))) (if rxy (* 64 (floor (+ s 32) 64)) s)) 0)))
                            (dotimes (i n)
                              (let ((x (aref cux i)) (y (aref cuy i)) (px (aref ccx i)) (py (aref ccy i)))
                                (push (+ (round (+ (* a x) (* c y)) 16384) (if xy dx 0)) uxs)
                                (push (+ (round (+ (* b x) (* e y)) 16384) (if xy dy 0)) uys)
                                (push (+ (mul2.14 px a) (mul2.14 py c) ox) cxs)
                                (push (+ (mul2.14 px b) (mul2.14 py e) oy) cys)
                                (push (aref con i) ons)))
                            (loop for ce across cend do (push (+ base ce) ends))
                            (incf base n))))))
                  (values (coerce (nreverse uxs) 'vector) (coerce (nreverse uys) 'vector)
                          (coerce (nreverse cxs) 'vector) (coerce (nreverse cys) 'vector)
                          (coerce (nreverse ons) 'vector) (coerce (nreverse ends) 'vector)))))))))

;;; ================================================================ glyph hinting
;;; ---- coordinate helpers (all F26.6) ----
(declaim (inline gc-cur gc-org gc-orus))
(defun gc-cur (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-cur-x z) i) (aref (hz-cur-y z) i) (hs-px h) (hs-py h))))
(defun gc-org (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-org-x z) i) (aref (hz-org-y z) i) (hs-dpx h) (hs-dpy h))))
;; dual-projection of the ORIGINAL UNSCALED (font-unit) coords — IP/IUP ratios use
;; these (like FreeType's `orus`) so precision isn't lost to per-point scale rounding.
(defun gc-orus (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-orus-x z) i) (aref (hz-orus-y z) i) (hs-dpx h) (hs-dpy h))))
;; FreeType measures point-to-point distances by projecting the DIFFERENCE vector
;; once (PROJECT / DUALPROJ), not by subtracting two independently-projected points
;; — those differ by a rounding unit and cascade into 1-unit mover errors.
(defun cur-dist (h zpp p zpq q)
  "PROJECT(cur[p] - cur[q])."
  (let ((zp (zone h zpp)) (zq (zone h zpq)))
    (dot14 (- (aref (hz-cur-x zp) p) (aref (hz-cur-x zq) q))
           (- (aref (hz-cur-y zp) p) (aref (hz-cur-y zq) q)) (hs-px h) (hs-py h))))
(defun org-dist (h zpp p zpq q)
  "DUALPROJ(org[p] - org[q]) on the scaled original coords (FreeType's MDRP/MIRP org)."
  (let ((zp (zone h zpp)) (zq (zone h zpq)))
    (dot14 (- (aref (hz-org-x zp) p) (aref (hz-org-x zq) q))
           (- (aref (hz-org-y zp) p) (aref (hz-org-y zq) q)) (hs-dpx h) (hs-dpy h))))
(defun orus-diff (h zpp p zpq q)
  "DUALPROJ(orus[p] - orus[q]) on the ORUS (font-unit) coords — IP's original ranges."
  (let ((zp (zone h zpp)) (zq (zone h zpq)))
    (dot14 (- (aref (hz-orus-x zp) p) (aref (hz-orus-x zq) q))
           (- (aref (hz-orus-y zp) p) (aref (hz-orus-y zq) q)) (hs-dpx h) (hs-dpy h))))
(defun orus-dist (h zp1 p zp0 rp0)
  "Original projected distance (MDRP/MIRP): DUAL-PROJECT the ORUS (font-unit)
difference, THEN scale — like FreeType (MulFix(DUALPROJ(orus-diff), x_scale), with
x_scale==y_scale).  Doing the dual-projection before scaling rounds once at the end,
which matters for diagonal projection vectors.  Twilight falls back to scaled org."
  (if (or (zerop zp0) (zerop zp1))
      (org-dist h zp1 p zp0 rp0)
      (hscale h (orus-diff h zp1 p zp0 rp0))))
(defun move (h zp i dist) (move-point h (zone h zp) i dist))
(defun touch-pt (h zp i)
  (let ((z (zone h zp)))
    (when (/= (hs-fx h) 0) (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 1)))
    (when (/= (hs-fy h) 0) (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 2)))))

;;; ---- load a glyph into zone 1 (+ 4 phantom points), zone 0 = twilight ----
(defun load-glyph-zone (h gid)
  "Populate zone 1 with GID's scaled points + phantom points; zone 0 twilight.
Handles simple AND composite glyphs (components merged via GLYPH-POINTS)."
  (let* ((font (hs-font h)) (d (font-data font)))
    (multiple-value-bind (off len) (loca-offset font gid)
      (when (zerop len) (return-from load-glyph-zone nil))
      (let ((g (+ (req-table font "glyf") off)))
        (multiple-value-bind (gux guy gcx gcy gon endpts) (glyph-points h gid)
          (let* ((np (length gux))
                 (total (+ np 4))
                 (cx (make-array total :element-type 'fixnum)) (cy (make-array total :element-type 'fixnum))
                 (ox (make-array total :element-type 'fixnum)) (oy (make-array total :element-type 'fixnum))
                 (ux (make-array total :element-type 'fixnum)) (uy (make-array total :element-type 'fixnum))
                 (on (make-array total :element-type 'bit)) (tc (make-array total :element-type 'fixnum :initial-element 0))
                 (xmin (s16 d (+ g 2))) (ymax (s16 d (+ g 8))))
            (dotimes (i np)
              (setf (aref cx i) (aref gcx i) (aref ox i) (aref gcx i)
                    (aref cy i) (aref gcy i) (aref oy i) (aref gcy i)
                    (aref ux i) (aref gux i) (aref uy i) (aref guy i)
                    (aref on i) (aref gon i)))
            ;; phantom points (FUnits): pp1 origin, pp2 advance, pp3/pp4 vertical
            (multiple-value-bind (adv lsb) (hmtx-metrics font gid)
              (let* ((p1x (hscale h (- xmin lsb))) (p2x (hscale h (+ (- xmin lsb) adv)))
                     (p3y (hscale h ymax)))
                (flet ((setp (i x y ufx ufy) (setf (aref cx i) x (aref ox i) x (aref cy i) y (aref oy i) y
                                                   (aref ux i) ufx (aref uy i) ufy (aref on i) 1)))
                  (setp np p1x 0 (- xmin lsb) 0) (setp (+ np 1) p2x 0 (+ (- xmin lsb) adv) 0)
                  (setp (+ np 2) 0 p3y 0 ymax) (setp (+ np 3) 0 0 0 0))
                ;; grid-fit the phantom points' CURRENT position (FT_PIX_ROUND);
                ;; ORG stays unrounded so IP/MDRP measure the true original metrics.
                ;; This is what lets IP shift a stem when the advance rounds (2.67->3.00).
                (flet ((pr (v) (* 64 (floor (+ v 32) 64))))
                  (dotimes (k 4) (let ((i (+ np k))) (setf (aref cx i) (pr (aref cx i)) (aref cy i) (pr (aref cy i))))))))
            (setf (hs-glyph-npts h) np (hs-glyph-ends h) endpts)
            (let ((glyph (make-hz :cur-x cx :cur-y cy :org-x ox :org-y oy :orus-x ux :orus-y uy :on on :touch tc))
                  (mtw (max 4 (1+ (or (getf (parse-maxp1 font) :max-twilight) 4)))))
              (setf (hs-zones h)
                    (vector (make-hz :cur-x (make-array mtw :element-type 'fixnum) :cur-y (make-array mtw :element-type 'fixnum)
                                     :org-x (make-array mtw :element-type 'fixnum) :org-y (make-array mtw :element-type 'fixnum)
                                     :orus-x (make-array mtw :element-type 'fixnum) :orus-y (make-array mtw :element-type 'fixnum)
                                     :on (make-array mtw :element-type 'bit) :touch (make-array mtw :element-type 'fixnum :initial-element 0))
                            glyph)))
            t))))))

(defun reset-glyph-gs (h)
  "Per-glyph graphics-state reset, matching FreeType: the policy fields (round mode,
cut-ins, min-distance, delta base/shift, flip, ...) come from the post-prep snapshot
so a glyph program's changes don't leak; but the vectors, zone pointers, reference
points and loop are reset to the FIXED default (x-axis, zone 1, rp 0, loop 1) every
glyph — NOT to whatever prep happened to leave them at."
  (when (hs-gs-default h) (restore-gs h))
  (setf (hs-px h) 16384 (hs-py h) 0 (hs-dpx h) 16384 (hs-dpy h) 0 (hs-fx h) 16384 (hs-fy h) 0
        (hs-rp0 h) 0 (hs-rp1 h) 0 (hs-rp2 h) 0 (hs-zp0 h) 1 (hs-zp1 h) 1 (hs-zp2 h) 1 (hs-loop h) 1))

(defparameter *hint-light* nil
  "Light (Y-only) hinting: grid-fit the outline vertically but keep the ORIGINAL
(fractional, unhinted) X.  Baseline/x-height/stem heights snap crisp, but horizontal
stems stay sub-pixel — smooth, even inter-letter rhythm like the browser's default
Linux rendering, instead of full hinting's crisp-but-choppy X grid-fit.")

(defun hint-glyph (h gid)
  "Grid-fit GID at the hinter's ppem.  Returns (values contours advance-px): the
hinted contours as lists of (x y on) in px, and the grid-fit horizontal advance
(phantom pp2−pp1).  Returns NIL for an empty glyph, or :composite for an
unsupported composite.  With *HINT-LIGHT*, X is the unhinted scaled coordinate."
  (let ((loaded (load-glyph-zone h gid)))
    (unless (eq loaded t) (return-from hint-glyph loaded))
    (reset-glyph-gs h)
    (let ((ins (glyph-instructions (hs-font h) gid)))
      (when ins (setf (hs-sp h) 0) (run-program h ins)))
    ;; extract hinted contour points + the hinted advance (phantom pp1..pp2).
    ;; light hinting keeps the original (unhinted) X so horizontal spacing is smooth.
    (let* ((z (zone h 1)) (ends (hs-glyph-ends h)) (np (hs-glyph-npts h))
           (xs (if *hint-light* (hz-org-x z) (hz-cur-x z))) (out '()) (start 0))
      (dotimes (c (length ends))
        (let ((end (aref ends c)) (pts '()))
          (loop for i from start to end do
            (push (list (/ (aref xs i) 64.0) (/ (aref (hz-cur-y z) i) 64.0) (= 1 (aref (hz-on z) i))) pts))
          (push (nreverse pts) out) (setf start (1+ end))))
      (values (nreverse out)
              (/ (- (aref (hz-cur-x z) (1+ np)) (aref (hz-cur-x z) np)) 64.0)))))

(defun hint-subglyph (h gid)
  "Hint GID standalone (own zone + instructions), returning its hinted real points as
(values cur-x cur-y orus-x orus-y on endpts) — used to place composite components.
Saves/restores the shared VM's zone + program state so it can nest."
  (let ((sz (hs-zones h)) (se (hs-glyph-ends h)) (sn (hs-glyph-npts h))
        (sc (hs-code h)) (sp (hs-pc h)) (ss (hs-sp h)))
    (unwind-protect
         (if (eq (load-glyph-zone h gid) t)
             (progn
               (reset-glyph-gs h)
               (let ((ins (glyph-instructions (hs-font h) gid)))
                 (when ins (setf (hs-sp h) 0) (run-program h ins)))
               (let* ((z (zone h 1)) (np (hs-glyph-npts h)))
                 (values (subseq (hz-cur-x z) 0 np) (subseq (hz-cur-y z) 0 np)
                         (subseq (hz-orus-x z) 0 np) (subseq (hz-orus-y z) 0 np)
                         (subseq (hz-on z) 0 np) (copy-seq (hs-glyph-ends h)))))
             (values #() #() #() #() #() #()))
      (setf (hs-zones h) sz (hs-glyph-ends h) se (hs-glyph-npts h) sn
            (hs-code h) sc (hs-pc h) sp (hs-sp h) ss))))

;;; ---- point-mover opcodes ----
(def-op #x2E (h)                                     ; MDAP[0] (touch only)
  (let ((p (hpop h))) (touch-pt h (hs-zp0 h) p) (setf (hs-rp0 h) p (hs-rp1 h) p)))
(def-op #x2F (h)                                     ; MDAP[1] (round to grid)
  (let* ((p (hpop h)) (cur (gc-cur h (hs-zp0 h) p)))
    (move h (hs-zp0 h) p (- (hround h cur) cur)) (setf (hs-rp0 h) p (hs-rp1 h) p)))

(defun miap (h round)
  (let* ((n (hpop h)) (p (hpop h)) (zp (hs-zp0 h)) (cvt (aref (hs-cvt h) n)))
    (when (zerop zp)                                 ; twilight: set position from CVT
      (let ((z (zone h 0)))
        (setf (aref (hz-org-x z) p) (mul2.14 cvt (hs-px h)) (aref (hz-org-y z) p) (mul2.14 cvt (hs-py h))
              (aref (hz-cur-x z) p) (aref (hz-org-x z) p) (aref (hz-cur-y z) p) (aref (hz-org-y z) p))))
    (let ((cur (gc-cur h zp p)))
      (when round
        (when (> (abs (- cvt cur)) (hs-cvt-cut-in h)) (setf cvt cur))
        (setf cvt (hround h cvt)))
      (move h zp p (- cvt cur)) (setf (hs-rp0 h) p (hs-rp1 h) p))))
(def-op #x3E (h) (miap h nil))                       ; MIAP[0]
(def-op #x3F (h) (miap h t))                         ; MIAP[1]

(defun min-distance (dist org mindist)
  "FreeType MDRP/MIRP minimum-distance clamp: keep DIST at least MINDIST in ORG's
direction (asymmetric — based on ORG's sign, not |dist|)."
  (if (>= org 0)
      (if (< dist mindist) mindist dist)
      (if (> dist (- mindist)) (- mindist) dist)))

(defun mdrp (h op)
  ;; flags: bit2 (0x04) round, bit3 (0x08) keep-min-distance, bit4 (0x10) set rp0.
  (let* ((p (hpop h)) (org (orus-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h)))
         (dist (if (logbitp 2 op) (hround h org) org)))
    (when (logbitp 3 op) (setf dist (min-distance dist org (hs-min-dist h))))
    (move h (hs-zp1 h) p (- dist (cur-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h))))
    (setf (hs-rp1 h) (hs-rp0 h) (hs-rp2 h) p) (when (logbitp 4 op) (setf (hs-rp0 h) p))))
(dotimes (i 32) (setf (aref *ops* (+ #xC0 i)) (lambda (h op) (mdrp h op))))  ; MDRP

(defun mirp (h op)
  ;; FreeType order: auto-flip cvt to org's sign FIRST, THEN the control-value cut-in
  ;; (same-zone), round, and min-distance.
  (let* ((n (hpop h)) (p (hpop h)) (cvt (aref (hs-cvt h) n))
         (org (orus-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h))))
    (when (and (hs-auto-flip h) (minusp (* org cvt))) (setf cvt (- cvt)))
    (let ((dist (if (logbitp 2 op)
                    (progn (when (and (= (hs-zp0 h) (hs-zp1 h))
                                      (> (abs (- cvt org)) (hs-cvt-cut-in h)))
                             (setf cvt org))
                           (hround h cvt))
                    cvt)))
      (when (logbitp 3 op) (setf dist (min-distance dist org (hs-min-dist h))))
      (move h (hs-zp1 h) p (- dist (cur-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h))))
      (setf (hs-rp1 h) (hs-rp0 h) (hs-rp2 h) p) (when (logbitp 4 op) (setf (hs-rp0 h) p)))))
(dotimes (i 32) (setf (aref *ops* (+ #xE0 i)) (lambda (h op) (mirp h op))))  ; MIRP

(defun msirp (h op)                                  ; MSIRP: move point to a stack distance from rp0
  (let ((distance (hpop h)) (point (hpop h)))
    (when (zerop (hs-zp1 h))                          ; twilight: seed point at rp0 + distance
      (let ((z0 (zone h (hs-zp0 h))) (z1 (zone h 0)) (rp0 (hs-rp0 h)))
        (setf (aref (hz-org-x z1) point) (+ (aref (hz-org-x z0) rp0) (mul2.14 distance (hs-fx h)))
              (aref (hz-org-y z1) point) (+ (aref (hz-org-y z0) rp0) (mul2.14 distance (hs-fy h)))
              (aref (hz-cur-x z1) point) (aref (hz-org-x z1) point)
              (aref (hz-cur-y z1) point) (aref (hz-org-y z1) point))))
    (move h (hs-zp1 h) point (- distance (cur-dist h (hs-zp1 h) point (hs-zp0 h) (hs-rp0 h))))
    (setf (hs-rp1 h) (hs-rp0 h) (hs-rp2 h) point)
    (when (logbitp 0 op) (setf (hs-rp0 h) point))))
(def-op #x3A (h op) (msirp h op)) (def-op #x3B (h op) (msirp h op))

(def-op #x39 (h)                                     ; IP
  ;; Ranges/distances are projections of DIFFERENCE vectors (FreeType PROJECT/
  ;; DUALPROJ): original in ORUS (font units) for precision, current in scaled cur.
  ;; Base = rp1.
  (let* ((ra (hs-rp1 h)) (rb (hs-rp2 h))
         (old-range (orus-diff h (hs-zp1 h) rb (hs-zp0 h) ra))
         (cur-range (cur-dist h (hs-zp1 h) rb (hs-zp0 h) ra)))
    (dotimes (k (hs-loop h))
      (let* ((p (hpop h))
             (org-dist (orus-diff h (hs-zp2 h) p (hs-zp0 h) ra))
             (cp (cur-dist h (hs-zp2 h) p (hs-zp0 h) ra))
             (new (if (zerop old-range) 0 (muldiv org-dist cur-range old-range))))
        (move h (hs-zp2 h) p (- new cp))))
    (setf (hs-loop h) 1)))

(defun sh-ref (h op)
  "FreeType Compute_Point_Displacement for SHP/SHC/SHZ: the reference is rp1/zp0 (odd
opcode) or rp2/zp1 (even); the shift is PROJECT(cur[ref] - org[ref]) — the projection
of the difference vector (then applied along the freedom vector by MOVE).  Returns
(values displacement ref-point ref-zone)."
  (multiple-value-bind (rp zref) (if (logbitp 0 op) (values (hs-rp1 h) (hs-zp0 h)) (values (hs-rp2 h) (hs-zp1 h)))
    (let ((z (zone h zref)))
      (values (dot14 (- (aref (hz-cur-x z) rp) (aref (hz-org-x z) rp))
                     (- (aref (hz-cur-y z) rp) (aref (hz-org-y z) rp)) (hs-px h) (hs-py h))
              rp zref))))
(defun shp (h op)                                    ; SHP[a]
  (multiple-value-bind (disp rp zref) (sh-ref h op) (declare (ignore rp zref))
    (dotimes (k (hs-loop h)) (move h (hs-zp2 h) (hpop h) disp))
    (setf (hs-loop h) 1)))
(def-op #x32 (h op) (shp h op)) (def-op #x33 (h op) (shp h op))
(defun shc (h op)                                    ; SHC: shift a whole contour
  (multiple-value-bind (disp rp zref) (sh-ref h op)
    (let* ((c (hpop h)) (ends (hs-glyph-ends h))
           (start (if (zerop c) 0 (1+ (aref ends (1- c))))) (end (aref ends c)))
      (loop for i from start to end
            unless (and (= (hs-zp2 h) zref) (= i rp)) do (move h (hs-zp2 h) i disp)))))
(def-op #x34 (h op) (shc h op)) (def-op #x35 (h op) (shc h op))
(defun shz (h op)                                    ; SHZ: shift an entire zone
  (multiple-value-bind (disp rp zref) (sh-ref h op)
    (let* ((e (hpop h)) (z (zone h e)) (n (length (hz-cur-x z))))
      (dotimes (i n) (unless (and (= e zref) (= i rp)) (move h e i disp))))))
(def-op #x36 (h op) (shz h op)) (def-op #x37 (h op) (shz h op))

(def-op #x38 (h)                                     ; SHPIX: shift loop points by a
  ;; pixel AMOUNT along the freedom vector (amount is on top, points below it).
  (let* ((amount (hpop h)) (dx (mul2.14 amount (hs-fx h))) (dy (mul2.14 amount (hs-fy h)))
         (z (zone h (hs-zp2 h))))
    (dotimes (k (hs-loop h))
      (let ((p (hpop h)))
        (incf (aref (hz-cur-x z) p) dx) (incf (aref (hz-cur-y z) p) dy)
        (touch-pt h (hs-zp2 h) p)))
    (setf (hs-loop h) 1)))

(def-op #x3C (h)                                     ; ALIGNRP
  (let ((rp0 (hs-rp0 h)))
    (dotimes (k (hs-loop h))
      (let ((p (hpop h))) (move h (hs-zp1 h) p (cur-dist h (hs-zp0 h) rp0 (hs-zp1 h) p))))
    (setf (hs-loop h) 1)))

(def-op #x46 (h) (hpush h (gc-cur h (hs-zp2 h) (hpop h))))   ; GC[0] current
(def-op #x47 (h) (hpush h (gc-org h (hs-zp2 h) (hpop h))))   ; GC[1] original
(def-op #x49 (h) (let ((b (hpop h)) (a (hpop h)))            ; MD[0] current distance
                   (hpush h (cur-dist h (hs-zp0 h) a (hs-zp1 h) b))))
(def-op #x4A (h) (let ((b (hpop h)) (a (hpop h)))            ; MD[1] original distance
                   (hpush h (orus-dist h (hs-zp0 h) a (hs-zp1 h) b))))
(def-op #x48 (h) (let ((v (hpop h)) (p (hpop h)))            ; SCFS
                   (move h (hs-zp2 h) p (- v (gc-cur h (hs-zp2 h) p)))))

;;; ---- IUP: interpolate untouched points per contour, per axis ----
(defun iup-axis (h touchbit cur org orus)
  "Interpolate untouched (in TOUCHBIT) points per contour.  Interior points use the
ORUS (font-unit) ratio like FreeType; points outside the touched span shift rigidly
by the nearest touched point's delta (scaled ORG)."
  (let* ((z (zone h 1)) (ends (hs-glyph-ends h)) (start 0) (tou (hz-touch z)))
    (dotimes (c (length ends))
      (let* ((end (aref ends c)) (n (1+ (- end start))))
        (when (plusp n)
          ;; collect touched indices in this contour
          (let ((touched '()))
            (loop for i from start to end when (logbitp (if (= touchbit 1) 0 1) (aref tou i)) do (push i touched))
            (setf touched (nreverse touched))
            (when touched
              (flet ((interp (i a b)                ; move untouched i between touched a,b
                       ;; Exact port of FreeType iup_worker_interpolate_: sort refs by
                       ;; ORUS; the boundary classification uses SCALED org (so points
                       ;; sharing a ref's scaled position shift with it); the interior
                       ;; ratio uses ORUS (font units) via DivFix->MulFix.
                       (multiple-value-bind (r1 r2)
                           (if (<= (funcall orus a) (funcall orus b)) (values a b) (values b a))
                         (let* ((orus1 (funcall orus r1)) (orus2 (funcall orus r2))
                                (org1 (funcall org r1)) (org2 (funcall org r2))
                                (cur1 (funcall cur r1)) (cur2 (funcall cur r2))
                                (xi (funcall org i)))
                           (funcall cur i
                                    (cond ((<= xi org1) (+ xi (- cur1 org1)))
                                          ((>= xi org2) (+ xi (- cur2 org2)))
                                          ((or (= cur1 cur2) (= orus1 orus2)) cur1)
                                          (t (+ cur1 (muldiv (- (funcall orus i) orus1)
                                                             (muldiv (- cur2 cur1) 65536 (- orus2 orus1))
                                                             65536)))))))))
                (if (= 1 (length touched))
                    ;; single touched point: shift whole contour by its delta
                    (let* ((a (car touched)) (delta (- (funcall cur a) (funcall org a))))
                      (loop for i from start to end unless (= i a) do (funcall cur i (+ (funcall org i) delta))))
                    ;; between each consecutive touched pair (with wraparound)
                    (loop for (a b) on (append touched (list (car touched)))
                          while b do
                            (loop for k from 1 below (mod (- b a) n)   ; untouched strictly between a..b (cyclic)
                                  for i = (+ start (mod (+ (- a start) k) n))
                                  do (interp i a b))))))))
        (setf start (1+ end))))))
(macrolet ((cur-acc (bit) `(lambda (&rest r) (let ((z (zone h 1)))
                             (if (cdr r) (setf (aref ,(if (= bit 1) '(hz-cur-x z) '(hz-cur-y z)) (first r)) (second r))
                                 (aref ,(if (= bit 1) '(hz-cur-x z) '(hz-cur-y z)) (first r)))))))
  (def-op #x30 (h) (iup-axis h 2 (let ((z (zone h 1))) (lambda (i &optional (v nil vp)) (if vp (setf (aref (hz-cur-y z) i) v) (aref (hz-cur-y z) i))))
                            (let ((z (zone h 1))) (lambda (i) (aref (hz-org-y z) i)))
                            (let ((z (zone h 1))) (lambda (i) (aref (hz-orus-y z) i)))))  ; IUP[y]
  (def-op #x31 (h) (iup-axis h 1 (let ((z (zone h 1))) (lambda (i &optional (v nil vp)) (if vp (setf (aref (hz-cur-x z) i) v) (aref (hz-cur-x z) i))))
                            (let ((z (zone h 1))) (lambda (i) (aref (hz-org-x z) i)))
                            (let ((z (zone h 1))) (lambda (i) (aref (hz-orus-x z) i))))))  ; IUP[x]

;;; ---- vector setters that need points (SPVTL/SFVTL) + misc ----
(defun norm14 (vx vy)
  "F2.14 unit vector of (vx,vy), an exact port of FreeType's Normalize +
FT_Vector_NormLen: Newton-Raphson normalize to length 0x10000 (16.16), then /4.
This matches FreeType's projection vectors bit-for-bit (isqrt was ~9/16384 off,
which cascaded into 1-unit diagonal MIRP errors)."
  (labels ((u32 (n) (logand n #xFFFFFFFF))
           (s32 (n) (let ((m (logand n #xFFFFFFFF))) (if (>= m #x80000000) (- m #x100000000) m))))
    (if (and (zerop vx) (zerop vy)) (values 16384 0)
        (let ((sx (if (minusp vx) -1 1)) (sy (if (minusp vy) -1 1)) (x (abs vx)) (y (abs vy)))
          (cond ((zerop x) (values 0 (if (plusp y) (* sy 16384) 0)))
                ((zerop y) (values (if (plusp x) (* sx 16384) 0) 0))
                (t
                 (let* ((l (if (> x y) (+ x (ash y -1)) (+ y (ash x -1))))
                        (shift (- 32 (integer-length l))))
                   (decf shift (+ 15 (if (>= l (ash #xAAAAAAAA (- shift))) 1 0)))
                   (if (> shift 0)
                       (progn (setf x (ash x shift) y (ash y shift))
                              (setf l (if (> x y) (+ x (ash y -1)) (+ y (ash x -1)))))
                       (setf x (ash x shift) y (ash y shift) l (ash l shift)))
                   (let ((b (- #x10000 l)) (x_ x) (y_ y) (u 0) (v 0) (z 0))
                     (loop
                       (setf u (u32 (+ x_ (ash (s32 (* x_ b)) -16)))
                             v (u32 (+ y_ (ash (s32 (* y_ b)) -16))))
                       (setf z (truncate (- (s32 (+ (* u u) (* v v)))) #x200))
                       (setf z (truncate (* z (ash (+ #x10000 b) -8)) #x10000))
                       (incf b z)
                       (when (<= z 0) (return)))
                     (values (truncate (* sx u) 4) (truncate (* sy v) 4))))))))))
(defun sxvtl (h op set-proj)
  "SPVTL/SFVTL: set the vector PARALLEL (opcode even) or PERPENDICULAR (opcode odd,
`opcode & 1`) to the line from point A (zp2) to point B (zp1), in current coords.
Perpendicular is a 90° CCW rotation, matching FreeType's Ins_SxVTL.  Sets proj+dual
(and also freedom for the SFVTL forms)."
  (let* ((pa (hpop h)) (pb (hpop h)) (za (zone h (hs-zp2 h))) (zb (zone h (hs-zp1 h)))
         (dx (- (aref (hz-cur-x zb) pb) (aref (hz-cur-x za) pa)))
         (dy (- (aref (hz-cur-y zb) pb) (aref (hz-cur-y za) pa))))
    (when (oddp op) (psetf dx (- dy) dy dx))          ; perpendicular (90° CCW)
    (multiple-value-bind (ux uy) (norm14 dx dy)
      (if set-proj (setf (hs-px h) ux (hs-py h) uy (hs-dpx h) ux (hs-dpy h) uy)
          (setf (hs-fx h) ux (hs-fy h) uy)))))
(def-op #x06 (h op) (sxvtl h op t))                  ; SPVTL[0] (parallel)
(def-op #x07 (h op) (sxvtl h op t))                  ; SPVTL[1] (perpendicular)
(def-op #x08 (h op) (sxvtl h op nil))                ; SFVTL[0] (parallel)
(def-op #x09 (h op) (sxvtl h op nil))                ; SFVTL[1] (perpendicular)
(def-op #x0F (h)                                     ; ISECT: move point to line-line intersection
  (let* ((b1 (hpop h)) (b0 (hpop h)) (a1 (hpop h)) (a0 (hpop h)) (pt (hpop h))
         (z0 (zone h (hs-zp0 h))) (z1 (zone h (hs-zp1 h))) (z2 (zone h (hs-zp2 h)))
         (dbx (- (aref (hz-cur-x z0) b1) (aref (hz-cur-x z0) b0)))
         (dby (- (aref (hz-cur-y z0) b1) (aref (hz-cur-y z0) b0)))
         (dax (- (aref (hz-cur-x z1) a1) (aref (hz-cur-x z1) a0)))
         (day (- (aref (hz-cur-y z1) a1) (aref (hz-cur-y z1) a0)))
         (dx (- (aref (hz-cur-x z0) b0) (aref (hz-cur-x z1) a0)))
         (dy (- (aref (hz-cur-y z0) b0) (aref (hz-cur-y z1) a0)))
         (disc (+ (muldiv dax (- dby) 64) (muldiv day dbx 64)))
         (dot (+ (muldiv dax dbx 64) (muldiv day dby 64))))
    (if (> (* 19 (abs disc)) (abs dot))
        (let* ((val (+ (muldiv dx (- dby) 64) (muldiv dy dbx 64)))
               (rx (muldiv val dax disc)) (ry (muldiv val day disc)))
          (setf (aref (hz-cur-x z2) pt) (+ (aref (hz-cur-x z1) a0) rx)
                (aref (hz-cur-y z2) pt) (+ (aref (hz-cur-y z1) a0) ry)))
        (setf (aref (hz-cur-x z2) pt) (ash (+ (aref (hz-cur-x z1) a0) (aref (hz-cur-x z1) a1)
                                             (aref (hz-cur-x z0) b0) (aref (hz-cur-x z0) b1)) -2)
              (aref (hz-cur-y z2) pt) (ash (+ (aref (hz-cur-y z1) a0) (aref (hz-cur-y z1) a1)
                                             (aref (hz-cur-y z0) b0) (aref (hz-cur-y z0) b1)) -2)))
    (setf (aref (hz-touch z2) pt) (logior (aref (hz-touch z2) pt) 3))))
(def-op #x0A (h) (let ((y (hpop h)) (x (hpop h))) (setf (hs-px h) x (hs-py h) y (hs-dpx h) x (hs-dpy h) y)))  ; SPVFS
(def-op #x0B (h) (let ((y (hpop h)) (x (hpop h))) (setf (hs-fx h) x (hs-fy h) y)))                            ; SFVFS
(def-op #x0C (h) (hpush h (hs-px h)) (hpush h (hs-py h)))    ; GPV
(def-op #x0D (h) (hpush h (hs-fx h)) (hpush h (hs-fy h)))    ; GFV
(def-op #x25 (h) (hpush h (aref (hs-stack h) (- (hs-sp h) 1 (hpop h)))))  ; CINDEX
(def-op #x26 (h) (let* ((k (hpop h)) (i (- (hs-sp h) k)) (v (aref (hs-stack h) i)))  ; MINDEX
                   (loop for j from i below (1- (hs-sp h)) do (setf (aref (hs-stack h) j) (aref (hs-stack h) (1+ j))))
                   (decf (hs-sp h)) (hpush h v)))

;;; ================================================ integration with rasterize-glyph
;;; A hinter (fpgm+prep run) is reused for every glyph at a given (font, ppem);
;;; hinted outlines are memoized per (font, gid, ppem).  RASTERIZE-GLYPH calls
;;; HINTED-SEGMENTS through the *HINTED-SEGMENTS-FN* hook when *HINTING* is on.
(defvar *hinter-cache* (make-hash-table :test 'equal)
  "(font . ppem) -> a hinter with fpgm+prep already run, or :NONE.")
(defvar *hinted-cache* (make-hash-table :test 'equal)
  "(font gid ppem) -> hinted segment lists (px), :EMPTY, or NIL (not hintable).")

(defun get-hinter (font ppem)
  "A cached hinter for FONT at integer PPEM (fpgm+prep run once), or :NONE if the
font has no working hint programs."
  (let ((key (cons font ppem)))
    (multiple-value-bind (v present) (gethash key *hinter-cache*)
      (if present v
          (setf (gethash key *hinter-cache*)
                (handler-case (let ((h (make-hinter font ppem))) (run-fpgm-prep h) h)
                  (error () :none)))))))

(defun hinted-segments (font gid ppem)
  "Grid-fit GID at integer PPEM.  Returns (values SEGS ADVANCE-PX), memoized: SEGS is
the hinted segment lists in px, :EMPTY for an outline-less glyph (space), or NIL when
it can't be hinted (caller uses the geometric path).  ADVANCE-PX is the grid-fit
advance so painted widths stay integer and even; NIL when not hinted."
  (let ((key (list font gid ppem *hint-light*)))
    (multiple-value-bind (v present) (gethash key *hinted-cache*)
      (if present (values (car v) (cdr v))
          (let ((h (get-hinter font ppem)))
            (if (eq h :none) (values nil nil)
                (handler-case
                    (multiple-value-bind (cs adv) (hint-glyph h gid)
                      (let* ((segs (cond ((null cs) :empty)          ; no outline (space)
                                         ((consp cs)
                                          (or (mapcan (lambda (c)
                                                        (let ((s (%contour->segments c))) (and s (list s))))
                                                      cs)
                                              :empty))
                                         (t nil)))                   ; unsupported -> fall back
                             ;; empty glyphs carry no phantom advance here — round the
                             ;; scaled hmtx advance so space is integer too.
                             (advance (if (numberp adv) (float adv 1d0)
                                          (float (round (* (advance-at font gid)
                                                           (/ ppem (font-units-per-em font)))) 1d0))))
                        (when segs (setf (gethash key *hinted-cache*) (cons segs advance)))
                        (values segs advance)))
                  (error () (values nil nil)))))))))

(defun hinted-advance (font gid ppem)
  "The grid-fit horizontal advance (px) for GID at integer PPEM, or NIL if the glyph
isn't hinted (caller uses the geometric advance).  For MEASURE = PAINT consistency."
  (nth-value 1 (hinted-segments font gid ppem)))

(defun clear-hint-caches ()
  "Drop the memoized hinters and hinted outlines (e.g. after changing fonts)."
  (clrhash *hinter-cache*) (clrhash *hinted-cache*))

(setf *hinted-segments-fn* #'hinted-segments)
