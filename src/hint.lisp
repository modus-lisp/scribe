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
(defun dot14 (ax ay bx by) (+ (mul2.14 ax bx) (mul2.14 ay by)))

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
  "Move point I of zone Z by DIST (F26.6) along the freedom vector; mark touched."
  (let* ((fx (hs-fx h)) (fy (hs-fy h))
         ;; distance along freedom vector so its projection == DIST
         (fdotp (dot14 fx fy (hs-px h) (hs-py h))))
    (unless (zerop fdotp)
      (when (/= fx 0) (incf (aref (hz-cur-x z) i) (muldiv dist fx fdotp))
            (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 1)))
      (when (/= fy 0) (incf (aref (hz-cur-y z) i) (muldiv dist fy fdotp))
            (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 2))))))

;;; ---- program bytes ----
(declaim (inline nextb))
(defun nextb (h) (prog1 (aref (hs-code h) (hs-pc h)) (incf (hs-pc h))))
(defun nextw (h) (let ((hi (nextb h)) (lo (nextb h)))
                   (let ((x (logior (ash hi 8) lo))) (if (>= x #x8000) (- x #x10000) x))))

;;; ---- the eval loop ----
(defun run-program (h code)
  "Execute CODE (a byte vector) to completion (pc past end)."
  (setf (hs-code h) code (hs-pc h) 0)
  (loop while (< (hs-pc h) (length code)) do (step-op h)))

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
(def-op #x62 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (muldiv a b 64)))) ; DIV
(def-op #x63 (h) (let ((b (hpop h)) (a (hpop h))) (hpush h (muldiv a b 64)))) ; MUL
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
               (step-op h))
      (setf (hs-code h) save-code (hs-pc h) save-pc))))
(def-op #x2B (h) (call-fn h (hpop h)))                                 ; CALL
(def-op #x2A (h) (let ((fn (hpop h)) (n (hpop h))) (dotimes (i n) (call-fn h fn)))) ; LOOPCALL

;;; ------------------------------------------------------- glyph loading
(defun glyph-instructions (font gid)
  "The per-glyph instruction byte stream (or NIL) for a simple glyph."
  (multiple-value-bind (off len) (loca-offset font gid)
    (when (plusp len)
      (let* ((d (font-data font)) (g (+ (req-table font "glyf") off)) (ncont (s16 d g)))
        (when (>= ncont 0)
          (let* ((p (+ g 10 (* 2 ncont))) (ilen (u16 d p)))
            (when (plusp ilen) (subseq d (+ p 2) (+ p 2 ilen)))))))))

;;; ================================================================ glyph hinting
;;; ---- coordinate helpers (all F26.6) ----
(declaim (inline gc-cur gc-org gc-orus))
(defun gc-cur (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-cur-x z) i) (aref (hz-cur-y z) i) (hs-px h) (hs-py h))))
(defun gc-org (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-org-x z) i) (aref (hz-org-y z) i) (hs-dpx h) (hs-dpy h))))
;; dual-projection of the ORIGINAL UNSCALED (font-unit) coords — IP/IUP ratios use
;; these (like FreeType's `orus`) so precision isn't lost to per-point scale rounding.
(defun gc-orus (h zp i) (let ((z (zone h zp))) (dot14 (aref (hz-orus-x z) i) (aref (hz-orus-y z) i) (hs-dpx h) (hs-dpy h))))
(defun orus-dist (h zp1 p zp0 rp0)
  "Original projected distance (p relative to rp0) as FreeType's MDRP/MIRP measure it:
scale the ORUS (font-unit) difference ONCE, then dual-project — avoids the extra
rounding of subtracting two independently-scaled coords.  Twilight falls back to org."
  (if (or (zerop zp0) (zerop zp1))
      (- (gc-org h zp1 p) (gc-org h zp0 rp0))
      (let* ((z1 (zone h zp1)) (z0 (zone h zp0))
             (dx (- (aref (hz-orus-x z1) p) (aref (hz-orus-x z0) rp0)))
             (dy (- (aref (hz-orus-y z1) p) (aref (hz-orus-y z0) rp0))))
        (dot14 (hscale h dx) (hscale h dy) (hs-dpx h) (hs-dpy h)))))
(defun move (h zp i dist) (move-point h (zone h zp) i dist))
(defun touch-pt (h zp i)
  (let ((z (zone h zp)))
    (when (/= (hs-fx h) 0) (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 1)))
    (when (/= (hs-fy h) 0) (setf (aref (hz-touch z) i) (logior (aref (hz-touch z) i) 2)))))

;;; ---- load a glyph into zone 1 (+ 4 phantom points), zone 0 = twilight ----
(defun load-glyph-zone (h gid)
  "Populate zone 1 with GID's scaled points + phantom points; zone 0 twilight."
  (let* ((font (hs-font h)) (d (font-data font)))
    (multiple-value-bind (off len) (loca-offset font gid)
      (when (zerop len) (return-from load-glyph-zone nil))
      (let* ((g (+ (req-table font "glyf") off)) (ncont (s16 d g)))
        (when (< ncont 0) (return-from load-glyph-zone :composite))
        (multiple-value-bind (xs ys flags endpts) (%simple-glyph d g ncont)
          (let* ((np (if (zerop ncont) 0 (1+ (aref endpts (1- ncont)))))
                 (total (+ np 4))
                 (cx (make-array total :element-type 'fixnum)) (cy (make-array total :element-type 'fixnum))
                 (ox (make-array total :element-type 'fixnum)) (oy (make-array total :element-type 'fixnum))
                 (ux (make-array total :element-type 'fixnum)) (uy (make-array total :element-type 'fixnum))
                 (on (make-array total :element-type 'bit)) (tc (make-array total :element-type 'fixnum :initial-element 0))
                 (xmin (s16 d (+ g 2))) (ymax (s16 d (+ g 8))))
            (dotimes (i np)
              (let ((sx (hscale h (aref xs i))) (sy (hscale h (aref ys i))))
                (setf (aref cx i) sx (aref ox i) sx (aref cy i) sy (aref oy i) sy
                      (aref ux i) (aref xs i) (aref uy i) (aref ys i)   ; unscaled (font units)
                      (aref on i) (if (logbitp 0 (aref flags i)) 1 0))))
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

(defun hint-glyph (h gid)
  "Grid-fit GID at the hinter's ppem; return the hinted contours as lists of
(x y on) in px (or NIL for empty / :composite for unsupported composites)."
  (let ((loaded (load-glyph-zone h gid)))
    (unless (eq loaded t) (return-from hint-glyph loaded))
    (reset-glyph-gs h)
    (let ((ins (glyph-instructions (hs-font h) gid)))
      (when ins (setf (hs-sp h) 0) (run-program h ins)))
    ;; extract hinted contour points
    (let* ((z (zone h 1)) (ends (hs-glyph-ends h)) (out '()) (start 0))
      (dotimes (c (length ends) (nreverse out))
        (let ((end (aref ends c)) (pts '()))
          (loop for i from start to end do
            (push (list (/ (aref (hz-cur-x z) i) 64.0) (/ (aref (hz-cur-y z) i) 64.0) (= 1 (aref (hz-on z) i))) pts))
          (push (nreverse pts) out) (setf start (1+ end)))))))

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

(defun mdrp (h op)
  ;; flags: bit2 (0x04) round, bit3 (0x08) keep-min-distance, bit4 (0x10) set rp0.
  (let* ((p (hpop h)) (org (orus-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h)))
         (dist (if (logbitp 2 op) (hround h org) org)))
    (when (logbitp 3 op)                             ; min distance
      (when (< (abs dist) (hs-min-dist h)) (setf dist (if (minusp org) (- (hs-min-dist h)) (hs-min-dist h)))))
    (let ((cur-rp0 (gc-cur h (hs-zp0 h) (hs-rp0 h))))
      (move h (hs-zp1 h) p (- (+ cur-rp0 dist) (gc-cur h (hs-zp1 h) p))))
    (setf (hs-rp1 h) (hs-rp0 h) (hs-rp2 h) p) (when (logbitp 4 op) (setf (hs-rp0 h) p))))
(dotimes (i 32) (setf (aref *ops* (+ #xC0 i)) (lambda (h op) (mdrp h op))))  ; MDRP

(defun mirp (h op)
  (let* ((n (hpop h)) (p (hpop h)) (cvt (aref (hs-cvt h) n))
         (org (orus-dist h (hs-zp1 h) p (hs-zp0 h) (hs-rp0 h))))
    (when (/= (hs-zp1 h) 0)                          ; cut-in (non-twilight)
      (when (> (abs (- cvt org)) (hs-cvt-cut-in h)) (setf cvt org)))
    (let ((dist (if (logbitp 2 op) (hround h cvt) cvt)))  ; bit2 round, bit3 min-dist
      (when (minusp org) (setf dist (- (abs dist))) )  ; sign follows original
      (unless (minusp org) (setf dist (abs dist)))
      (when (logbitp 3 op)
        (when (< (abs dist) (hs-min-dist h)) (setf dist (if (minusp org) (- (hs-min-dist h)) (hs-min-dist h)))))
      (let ((cur-rp0 (gc-cur h (hs-zp0 h) (hs-rp0 h))))
        (move h (hs-zp1 h) p (- (+ cur-rp0 dist) (gc-cur h (hs-zp1 h) p))))
      (setf (hs-rp1 h) (hs-rp0 h) (hs-rp2 h) p) (when (logbitp 4 op) (setf (hs-rp0 h) p)))))
(dotimes (i 32) (setf (aref *ops* (+ #xE0 i)) (lambda (h op) (mirp h op))))  ; MIRP

(def-op #x39 (h)                                     ; IP
  ;; original ranges/distances use ORUS (font units) like FreeType, so the ratio
  ;; keeps full precision; current uses the scaled cur coords.  Base = rp1.
  (let* ((ra (hs-rp1 h)) (rb (hs-rp2 h))
         (oa (gc-orus h (hs-zp0 h) ra)) (ob (gc-orus h (hs-zp1 h) rb))
         (ca (gc-cur h (hs-zp0 h) ra)) (cb (gc-cur h (hs-zp1 h) rb))
         (old-range (- ob oa)) (cur-range (- cb ca)))
    (dotimes (k (hs-loop h))
      (let* ((p (hpop h)) (op (gc-orus h (hs-zp2 h) p)) (cp (gc-cur h (hs-zp2 h) p))
             (org-dist (- op oa))
             (new (+ ca (if (zerop old-range) 0 (muldiv org-dist cur-range old-range)))))
        (move h (hs-zp2 h) p (- new cp))))
    (setf (hs-loop h) 1)))

(defun shp (h op)                                    ; SHP[a]
  (multiple-value-bind (rp zref) (if (logbitp 0 op) (values (hs-rp1 h) (hs-zp0 h)) (values (hs-rp2 h) (hs-zp1 h)))
    (let ((disp (- (gc-cur h zref rp) (gc-org h zref rp))))
      (dotimes (k (hs-loop h)) (move h (hs-zp2 h) (hpop h) disp))
      (setf (hs-loop h) 1))))
(def-op #x32 (h op) (shp h op)) (def-op #x33 (h op) (shp h op))

(defun sh-ref (h op)
  "The (displacement . zone-of-ref) for SHC/SHZ: like SHP, rp1/zp0 (odd) or rp2/zp1."
  (multiple-value-bind (rp zref) (if (logbitp 0 op) (values (hs-rp1 h) (hs-zp0 h)) (values (hs-rp2 h) (hs-zp1 h)))
    (values (- (gc-cur h zref rp) (gc-org h zref rp)) rp zref)))
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
      (let ((p (hpop h))) (move h (hs-zp1 h) p (- (gc-cur h (hs-zp0 h) rp0) (gc-cur h (hs-zp1 h) p)))))
    (setf (hs-loop h) 1)))

(def-op #x46 (h) (hpush h (gc-cur h (hs-zp2 h) (hpop h))))   ; GC[0] current
(def-op #x47 (h) (hpush h (gc-org h (hs-zp2 h) (hpop h))))   ; GC[1] original
(def-op #x49 (h) (let ((b (hpop h)) (a (hpop h)))            ; MD[0] current distance
                   (hpush h (- (gc-cur h (hs-zp0 h) a) (gc-cur h (hs-zp1 h) b)))))
(def-op #x4A (h) (let ((b (hpop h)) (a (hpop h)))            ; MD[1] original distance
                   (hpush h (- (gc-org h (hs-zp0 h) a) (gc-org h (hs-zp1 h) b)))))
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
                       (let ((ua (funcall orus a)) (ub (funcall orus b)) (ui (funcall orus i))
                             (ca (funcall cur a)) (cb (funcall cur b)))
                         (funcall cur i
                                  ;; interior: base on the SMALLER-orus reference (as
                                  ;; FreeType does) so the single MulDiv rounds identically.
                                  (cond ((and (<= (min ua ub) ui) (<= ui (max ua ub)))
                                         (cond ((= ua ub) ca)
                                               ((<= ua ub) (+ ca (muldiv (- ui ua) (- cb ca) (- ub ua))))
                                               (t (+ cb (muldiv (- ui ub) (- ca cb) (- ua ub))))))
                                        ((<= ui (min ua ub))
                                         (+ (funcall org i) (- (if (< ua ub) ca cb) (funcall org (if (< ua ub) a b)))))
                                        (t (+ (funcall org i) (- (if (> ua ub) ca cb) (funcall org (if (> ua ub) a b))))))))))
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
  "Normalize (vx,vy) F26.6 to an F2.14 unit vector (FreeType Normalize)."
  (cond ((and (zerop vx) (zerop vy)) (values 16384 0))
        ((zerop vx) (values 0 (if (minusp vy) -16384 16384)))
        ((zerop vy) (values (if (minusp vx) -16384 16384) 0))
        (t (let ((w (isqrt (+ (* vx vx) (* vy vy)))))
             (values (muldiv vx 16384 w) (muldiv vy 16384 w))))))
(defun sxvtl (h op set-proj)
  "SPVTL/SFVTL: vector along (parallel, opcode odd) or perpendicular (even) to the
line from point A (zp2) to point B (zp1), in current coords.  Sets proj+dual (and
also freedom for SFVTL forms)."
  (let* ((pa (hpop h)) (pb (hpop h)) (za (zone h (hs-zp2 h))) (zb (zone h (hs-zp1 h)))
         (dx (- (aref (hz-cur-x zb) pb) (aref (hz-cur-x za) pa)))
         (dy (- (aref (hz-cur-y zb) pb) (aref (hz-cur-y za) pa))))
    (when (evenp op) (psetf dx (- dy) dy dx))        ; perpendicular
    (multiple-value-bind (ux uy) (norm14 dx dy)
      (if set-proj (setf (hs-px h) ux (hs-py h) uy (hs-dpx h) ux (hs-dpy h) uy)
          (setf (hs-fx h) ux (hs-fy h) uy)))))
(def-op #x06 (h op) (sxvtl h op t))                  ; SPVTL[0] (perpendicular)
(def-op #x07 (h op) (sxvtl h op t))                  ; SPVTL[1] (parallel)
(def-op #x08 (h op) (sxvtl h op nil))                ; SFVTL[0] (perpendicular)
(def-op #x09 (h op) (sxvtl h op nil))                ; SFVTL[1] (parallel)
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
