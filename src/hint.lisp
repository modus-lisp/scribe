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

(defun run-fpgm-prep (h)
  "Run the font program (defines functions) then the control-value program (sets
graphics state / CVT for this ppem)."
  (let ((fp (table-bytes (hs-font h) "fpgm"))) (when fp (run-program h fp)))
  (let ((pp (table-bytes (hs-font h) "prep"))) (when pp (run-program h pp))))

;;; ------------------------------------------------------------- fixed point
;;; F26Dot6: px*64.  F2Dot14: unit vector components (16384 = 1.0).
(declaim (inline f26.6 muldiv))
(defun f26.6 (px) (round (* px 64)))
(defun muldiv (a b c)
  "round(a*b/c) with .5 rounding away from zero — FreeType's FT_MulDiv."
  (if (zerop c) 0
      (let* ((s (if (minusp (* a b)) -1 1)) (n (+ (abs (* a b)) (floor (abs c) 2))))
        (* s (floor n (abs c))))))
(declaim (inline mul2.14 dot14))
(defun mul2.14 (a b) (muldiv a b 16384))           ; F26.6 * F2.14 -> F26.6
(defun dot14 (ax ay bx by) (+ (mul2.14 ax bx) (mul2.14 ay by)))

;;; ------------------------------------------------------------- interpreter
(defstruct hz                                     ; a zone of points
  (cur-x nil) (cur-y nil)                          ; current coords, F26.6 arrays
  (org-x nil) (org-y nil)                          ; original (scaled) coords
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
  zones)                                            ; #(twilight glyph)

(define-condition hint-unimplemented (error)
  ((op :initarg :op :reader hint-op))
  (:report (lambda (c s) (format s "hint: unimplemented opcode #x~2,'0X" (hint-op c)))))

;;; ---- stack helpers ----
(declaim (inline hpush hpop))
(defun hpush (h v) (setf (aref (hs-stack h) (hs-sp h)) v) (incf (hs-sp h)))
(defun hpop (h) (decf (hs-sp h)) (aref (hs-stack h) (hs-sp h)))

;;; ---- rounding engine (round-state -> round an F26.6 distance) ----
(defun hround (h dist)
  "Round F26.6 DIST per the graphics-state round mode, preserving sign and (per
spec) never rounding a nonzero distance to zero."
  (if (eq (hs-round-state h) :off) dist
      (let* ((per (hs-round-period h)) (ph (hs-round-phase h)) (thr (hs-round-threshold h))
             (sign (if (minusp dist) -1 1)) (d (abs dist))
             (r (+ (* (floor (+ (- d ph) thr) per) per) ph)))
        (when (minusp r) (setf r 0))
        (* sign (if (zerop r) per r)))))

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
(defun do-deltac (h base-off)
  (let ((n (hpop h)) (base (+ (hs-delta-base h) base-off)))
    (dotimes (i n)
      (let ((arg (hpop h)) (idx (hpop h)))
        (when (= (hs-ppem h) (+ base (ash arg -4)))
          (incf (aref (hs-cvt h) idx) (delta-step h arg)))))))
(defun do-deltap (h base-off)
  (let ((n (hpop h)) (base (+ (hs-delta-base h) base-off)))
    (dotimes (i n)
      (let ((arg (hpop h)) (pt (hpop h)))
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
(def-op #x02 (h) (setf (hs-px h) 16384 (hs-py h) 0 (hs-dpx h) 16384 (hs-dpy h) 0)) ; SPVTCA[x]
(def-op #x03 (h) (setf (hs-px h) 0 (hs-py h) 16384 (hs-dpx h) 0 (hs-dpy h) 16384)) ; SPVTCA[y]
(def-op #x04 (h) (setf (hs-fx h) 16384 (hs-fy h) 0))                     ; SFVTCA[x]
(def-op #x05 (h) (setf (hs-fx h) 0 (hs-fy h) 16384))                     ; SFVTCA[y]
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
(def-op #x7C (h) (setf (hs-round-state h) :grid)   (set-round-period h 64 0 64))  ; RUTG (up)
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
