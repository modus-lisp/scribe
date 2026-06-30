;;;; woff.lisp — WOFF / WOFF2 web-font decompression to sfnt (pure CL, clean-room).
;;;;
;;;; scribe:open-font transparently accepts .woff2 / .woff bytes: detect the
;;;; signature, decompress to a standard in-memory sfnt, then parse as normal.
;;;;
;;;;   wOFF (0x774F4646) -> WOFF1: per-table zlib (deflate:zlib-decompress)
;;;;   wOF2 (0x774F4632) -> WOFF2: single brotli block + glyf/loca transform
;;;;
;;;; Compression deps only: brotli-pure (WOFF2 block), deflate (WOFF1 tables).
;;;; Everything else — header/dir/UIntBase128/255UInt16/triplet/glyf-rebuild/
;;;; sfnt-reassembly — is our own code, per the W3C WOFF2 spec §5.2.
(in-package #:scribe)

(deftype ub8v () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline make-ub8v))
(defun make-ub8v (n) (make-array n :element-type '(unsigned-byte 8)))

;;; ===========================================================================
;;; A tiny growable big-endian byte writer (for assembling sfnt + glyf streams).
;;; ===========================================================================
(defstruct (bw (:constructor %make-bw))
  (buf (make-array 4096 :element-type '(unsigned-byte 8)) :type ub8v)
  (n 0 :type fixnum))

(defun make-bw (&optional (cap 4096)) (%make-bw :buf (make-ub8v cap) :n 0))

(declaim (inline bw-ensure))
(defun bw-ensure (w extra)
  (let* ((buf (bw-buf w)) (cap (length buf)) (need (+ (bw-n w) extra)))
    (when (> need cap)
      (let ((nc cap)) (loop while (< nc need) do (setf nc (* 2 nc)))
        (let ((nb (make-ub8v nc)))
          (replace nb buf :end2 (bw-n w))
          (setf (bw-buf w) nb))))))

(defun bw-u8 (w v) (bw-ensure w 1) (setf (aref (bw-buf w) (bw-n w)) (logand v #xff)) (incf (bw-n w)))
(defun bw-u16 (w v) (bw-u8 w (ash v -8)) (bw-u8 w v))
(defun bw-s16 (w v) (bw-u16 w (logand v #xffff)))
(defun bw-u32 (w v) (bw-u16 w (ash v -16)) (bw-u16 w (logand v #xffff)))
(defun bw-bytes (w src &optional (start 0) (end (length src)))
  (let ((len (- end start)))
    (bw-ensure w len)
    (replace (bw-buf w) src :start1 (bw-n w) :start2 start :end2 end)
    (incf (bw-n w) len)))
(defun bw-pad4 (w) (loop until (zerop (mod (bw-n w) 4)) do (bw-u8 w 0)))
(defun bw-octets (w) (subseq (bw-buf w) 0 (bw-n w)))

;;; ===========================================================================
;;; A sequential big-endian reader over a byte vector (with a cursor).
;;; ===========================================================================
(defstruct (br (:constructor %make-br))
  (d (make-ub8v 0) :type ub8v)
  (p 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-br (d &optional (start 0) (end (length d)))
  (%make-br :d (coerce d '(simple-array (unsigned-byte 8) (*))) :p start :end end))

(defun br-u8 (r)
  (when (>= (br-p r) (br-end r)) (error "woff: read past end"))
  (prog1 (aref (br-d r) (br-p r)) (incf (br-p r))))
(defun br-u16 (r) (logior (ash (br-u8 r) 8) (br-u8 r)))
(defun br-s16 (r) (let ((v (br-u16 r))) (if (>= v #x8000) (- v #x10000) v)))
(defun br-u32 (r) (logior (ash (br-u16 r) 16) (br-u16 r)))
(defun br-bytes (r n)
  (let ((s (subseq (br-d r) (br-p r) (+ (br-p r) n)))) (incf (br-p r) n) s))
(defun br-remaining (r) (- (br-end r) (br-p r)))

;;; UIntBase128 (WOFF2 §4.1.1) — big-endian base-128, 1..5 bytes.
(defun br-base128 (r)
  (when (= (aref (br-d r) (br-p r)) #x80)
    (error "woff2: UIntBase128 must not start with leading zeros"))
  (let ((result 0))
    (dotimes (i 5 (error "woff2: UIntBase128 longer than 5 bytes"))
      (let ((code (br-u8 r)))
        (when (logtest result #xFE000000)
          (error "woff2: UIntBase128 exceeds 2^32-1"))
        (setf result (logior (ash result 7) (logand code #x7f)))
        (when (zerop (logand code #x80))
          (return result))))))

;;; 255UInt16 (WOFF2 §6.1.1) — 1..3 bytes.
(defun br-255ushort (r)
  (let ((code (br-u8 r)))
    (cond ((= code 253) (br-u16 r))
          ((= code 254) (+ (br-u8 r) 506))
          ((= code 255) (+ (br-u8 r) 253))
          (t code))))

;;; ===========================================================================
;;; sfnt assembly: build a standard offset-table + table directory + data.
;;; TABLES = list of (tag-string . octet-vector), data padded to 4 bytes.
;;; ===========================================================================
(defun assemble-sfnt (sfnt-version tables)
  (let* ((tables (sort (copy-list tables) #'string< :key #'car))
         (n (length tables))
         ;; searchRange = (largest power of 2 <= n) * 16
         (pow2 (let ((p 1)) (loop while (<= (* p 2) n) do (setf p (* p 2))) p))
         (search-range (* pow2 16))
         (entry-selector (let ((e 0) (p 1)) (loop while (<= (* p 2) n) do (setf p (* p 2)) (incf e)) e))
         (range-shift (- (* n 16) search-range))
         (w (make-bw (* 1 1024 1024))))
    ;; offset table
    (bw-u32 w sfnt-version)
    (bw-u16 w n) (bw-u16 w search-range) (bw-u16 w entry-selector) (bw-u16 w range-shift)
    ;; table directory — offsets computed after the directory.
    (let* ((dir-start (bw-n w))
           (data-start (+ dir-start (* n 16)))
           (offset data-start)
           (offsets '()))
      (dolist (tbl tables)
        (let ((len (length (cdr tbl))))
          (push (cons offset len) offsets)
          (incf offset (* 4 (ceiling len 4)))))
      (setf offsets (nreverse offsets))
      ;; write directory entries (checksum 0 — scribe does not verify checksums)
      (loop for tbl in tables for off in offsets do
        (let ((tagstr (car tbl)))
          (dotimes (i 4) (bw-u8 w (if (< i (length tagstr)) (char-code (char tagstr i)) 32)))
          (bw-u32 w 0)                 ; checksum
          (bw-u32 w (car off))         ; offset
          (bw-u32 w (cdr off))))       ; length
      ;; write table data, padded
      (loop for tbl in tables do
        (bw-bytes w (cdr tbl))
        (bw-pad4 w)))
    (bw-octets w)))

;;; ===========================================================================
;;; WOFF1
;;; ===========================================================================
(defun woff1-decode (bytes)
  "Decode a WOFF (version 1) font to a standard sfnt byte vector."
  (let* ((r (make-br bytes))
         (sig (br-u32 r)))
    (declare (ignore sig))
    (let* ((flavor (br-u32 r)))
      (br-u32 r)                       ; length
      (let ((num-tables (br-u16 r)))
        (br-u16 r)                     ; reserved
        (br-u32 r)                     ; totalSfntSize
        (br-u16 r) (br-u16 r)          ; major/minor version
        (br-u32 r) (br-u32 r) (br-u32 r) ; meta off/len/origLen
        (br-u32 r) (br-u32 r)          ; priv off/len
        ;; table directory: 20 bytes each
        (let ((d (br-d r)) (tables '()))
          (dotimes (i num-tables)
            (let* ((rec (+ 44 (* i 20)))
                   (tagstr (map 'string #'code-char (subseq d rec (+ rec 4))))
                   (off (u32 d (+ rec 4)))
                   (comp-len (u32 d (+ rec 8)))
                   (orig-len (u32 d (+ rec 12))))
              (let* ((raw (subseq d off (+ off comp-len)))
                     (data (if (< comp-len orig-len)
                               (deflate:zlib-decompress raw)
                               raw)))
                (unless (= (length data) orig-len)
                  (error "woff1: table ~a length ~d != origLength ~d"
                         tagstr (length data) orig-len))
                (push (cons tagstr data) tables))))
          (assemble-sfnt flavor (nreverse tables)))))))

;;; ===========================================================================
;;; WOFF2
;;; ===========================================================================
(defparameter *woff2-known-tags*
  #("cmap" "head" "hhea" "hmtx" "maxp" "name" "OS/2" "post" "cvt " "fpgm"
    "glyf" "loca" "prep" "CFF " "VORG" "EBDT" "EBLC" "gasp" "hdmx" "kern"
    "LTSH" "PCLT" "VDMX" "vhea" "vmtx" "BASE" "GDEF" "GPOS" "GSUB" "EBSC"
    "JSTF" "MATH" "CBDT" "CBLC" "COLR" "CPAL" "SVG " "sbix" "acnt" "avar"
    "bdat" "bloc" "bsln" "cvar" "fdsc" "feat" "fmtx" "fvar" "gvar" "hsty"
    "just" "lcar" "mort" "morx" "opbd" "prop" "trak" "Zapf" "Silf" "Glat"
    "Gloc" "Feat" "Sill")
  "63-entry knownTags index (WOFF2 §5).")

;; A decoded WOFF2 table directory entry.
(defstruct w2ent tag flags transformed orig-length transform-length data)

(defun woff2-decode (bytes)
  "Decode a WOFF2 font to a standard sfnt byte vector."
  (let* ((r (make-br bytes)))
    (br-u32 r)                          ; signature
    (let* ((flavor (br-u32 r)))
      (br-u32 r)                        ; length
      (let* ((num-tables (br-u16 r)))
        (br-u16 r)                      ; reserved
        (br-u32 r)                      ; totalSfntSize
        (let ((total-comp (br-u32 r)))
          (br-u16 r) (br-u16 r)         ; major/minor version
          (br-u32 r) (br-u32 r) (br-u32 r) ; meta off/len/origLen
          (br-u32 r) (br-u32 r)         ; priv off/len
          ;; ---- table directory ----
          (let ((ents (make-array num-tables)))
            (dotimes (i num-tables)
              (let* ((flags (br-u8 r))
                     (tag-idx (logand flags #x3f))
                     (xform-ver (ash (logand flags #xc0) -6))
                     (tag (if (= tag-idx #x3f)
                              (map 'string #'code-char (br-bytes r 4))
                              (aref *woff2-known-tags* tag-idx)))
                     ;; transform applies iff glyf/loca AND version 0.
                     (transformed (and (member tag '("glyf" "loca") :test #'string=)
                                       (= xform-ver 0)))
                     (orig-len (br-base128 r))
                     (xform-len (when transformed (br-base128 r))))
                (setf (aref ents i)
                      (make-w2ent :tag tag :flags flags :transformed transformed
                                  :orig-length orig-len :transform-length xform-len))))
            ;; ---- brotli-compressed block ----
            (let* ((comp (br-bytes r total-comp))
                   (decompressed (coerce (brotli-pure:decompress comp)
                                         '(simple-array (unsigned-byte 8) (*))))
                   (pos 0))
              ;; split the decompressed stream into per-table slices (no padding).
              (loop for e across ents do
                (let ((len (if (w2ent-transformed e)
                               (w2ent-transform-length e)
                               (w2ent-orig-length e))))
                  (setf (w2ent-data e) (subseq decompressed pos (+ pos len)))
                  (incf pos len)))
              ;; ---- reverse glyf transform (rebuild glyf + loca) ----
              (let ((glyf-e (find "glyf" ents :key #'w2ent-tag :test #'string=))
                    (loca-e (find "loca" ents :key #'w2ent-tag :test #'string=)))
                (when (and glyf-e (w2ent-transformed glyf-e))
                  (multiple-value-bind (glyf-bytes loca-bytes index-format)
                      (woff2-reconstruct-glyf (w2ent-data glyf-e))
                    (setf (w2ent-data glyf-e) glyf-bytes)
                    (when loca-e (setf (w2ent-data loca-e) loca-bytes))
                    ;; patch head.indexToLocFormat to match our emitted loca.
                    (let ((head-e (find "head" ents :key #'w2ent-tag :test #'string=)))
                      (when head-e
                        (u16-into (w2ent-data head-e) 50 index-format))))))
              ;; ---- assemble sfnt ----
              (assemble-sfnt flavor
                             (loop for e across ents
                                   collect (cons (w2ent-tag e) (w2ent-data e)))))))))))

(defun u16-into (vec off val)
  "Store VAL as a big-endian u16 at OFF in VEC; returns VAL."
  (setf (aref vec off) (logand (ash val -8) #xff)
        (aref vec (1+ off)) (logand val #xff))
  val)

;;; ---------------------------------------------------------------------------
;;; glyf transform reversal (WOFF2 §5.2). Returns (values glyf loca indexFormat).
;;; ---------------------------------------------------------------------------
(defparameter +overlap-simple-bitmap-flag+ 1)

(defun woff2-reconstruct-glyf (xglyf)
  (let* ((r (make-br xglyf)))
    (br-u16 r)                           ; reserved
    (let* ((option-flags (br-u16 r))
           (num-glyphs (br-u16 r))
           (index-format (br-u16 r))
           (n-contour-size (br-u32 r))
           (n-points-size (br-u32 r))
           (flag-size (br-u32 r))
           (glyph-size (br-u32 r))
           (composite-size (br-u32 r))
           (bbox-size (br-u32 r))
           (instruction-size (br-u32 r))
           ;; substreams follow the header, concatenated in order.
           (base (br-p r))
           (n-contour (make-br xglyf base (+ base n-contour-size)))
           (off1 (+ base n-contour-size))
           (n-points (make-br xglyf off1 (+ off1 n-points-size)))
           (off2 (+ off1 n-points-size))
           (flag-stream (make-br xglyf off2 (+ off2 flag-size)))
           (off3 (+ off2 flag-size))
           (glyph-stream (make-br xglyf off3 (+ off3 glyph-size)))
           (off4 (+ off3 glyph-size))
           (composite-stream (make-br xglyf off4 (+ off4 composite-size)))
           (off5 (+ off4 composite-size))
           (bbox-stream (make-br xglyf off5 (+ off5 bbox-size)))
           (off6 (+ off5 bbox-size))
           (instruction-stream (make-br xglyf off6 (+ off6 instruction-size)))
           (off7 (+ off6 instruction-size))
           (overlap-bitmap (when (logtest option-flags +overlap-simple-bitmap-flag+)
                             (make-br xglyf off7 (+ off7 (ceiling num-glyphs 8))))))
      (declare (ignore overlap-bitmap))
      ;; bboxStream begins with a bitmap padded to a 4-byte (32-bit) boundary.
      (let* ((bbox-bitmap-size (ash (ash (+ num-glyphs 31) -5) 2))
             (bbox-bitmap (br-bytes bbox-stream bbox-bitmap-size))
             ;; output: per-glyph standard glyf bytes; loca offsets.
             (glyf-w (make-bw (* 2 (length xglyf))))
             (loca-offsets (make-array (1+ num-glyphs))))
        (flet ((has-bbox (gid)
                 (logtest (aref bbox-bitmap (ash gid -3)) (ash #x80 (- (logand gid 7))))))
          (dotimes (gid num-glyphs)
            (setf (aref loca-offsets gid) (bw-n glyf-w))
            (let ((ncont (br-s16 n-contour)))
              (cond
                ((zerop ncont) nil)      ; empty glyph -> 0 bytes
                ((> ncont 0)
                 (woff2-emit-simple-glyph
                  glyf-w ncont n-points flag-stream glyph-stream instruction-stream
                  bbox-stream (has-bbox gid)))
                (t                       ; composite
                 (woff2-emit-composite-glyph
                  glyf-w composite-stream glyph-stream instruction-stream bbox-stream)))
              ;; pad each glyph to a 2-byte boundary.
              (when (oddp (bw-n glyf-w)) (bw-u8 glyf-w 0))))
          (setf (aref loca-offsets num-glyphs) (bw-n glyf-w)))
        ;; build loca per indexFormat.
        (let ((loca-w (make-bw (* (1+ num-glyphs) 4))))
          (if (= index-format 0)
              (dotimes (i (1+ num-glyphs)) (bw-u16 loca-w (ash (aref loca-offsets i) -1)))
              (dotimes (i (1+ num-glyphs)) (bw-u32 loca-w (aref loca-offsets i))))
          (values (bw-octets glyf-w) (bw-octets loca-w) index-format))))))

;;; --- triplet coordinate decode (WOFF2 §5.2, reference: fontTools _decodeTriplets)
(declaim (inline tri-sign))
(defun tri-sign (flag baseval) (if (logbitp 0 flag) baseval (- baseval)))

(defun woff2-decode-triplets (n-points flag-stream glyph-stream)
  "Decode N-POINTS points. Reads N-POINTS flag bytes from FLAG-STREAM and the
   coordinate bytes from GLYPH-STREAM. Returns (values xs ys on-curves) arrays."
  (let ((xs (make-array n-points)) (ys (make-array n-points))
        (oncv (make-array n-points)) (x 0) (y 0))
    (dotimes (i n-points)
      (let* ((flag (br-u8 flag-stream))
             (on-curve (not (logbitp 7 flag))))
        (setf flag (logand flag #x7f))
        (let (dx dy)
          (cond
            ((< flag 10)
             (setf dx 0
                   dy (tri-sign flag (+ (ash (logand flag 14) 7) (br-u8 glyph-stream)))))
            ((< flag 20)
             (setf dx (tri-sign flag (+ (ash (logand (- flag 10) 14) 7) (br-u8 glyph-stream)))
                   dy 0))
            ((< flag 84)
             (let* ((b0 (- flag 20)) (b1 (br-u8 glyph-stream)))
               (setf dx (tri-sign flag (+ 1 (logand b0 #x30) (ash b1 -4)))
                     dy (tri-sign (ash flag -1) (+ 1 (ash (logand b0 #x0c) 2) (logand b1 #x0f))))))
            ((< flag 120)
             (let* ((b0 (- flag 84)) (t0 (br-u8 glyph-stream)) (t1 (br-u8 glyph-stream)))
               (setf dx (tri-sign flag (+ 1 (ash (floor b0 12) 8) t0))
                     dy (tri-sign (ash flag -1) (+ 1 (ash (ash (mod b0 12) -2) 8) t1)))))
            ((< flag 124)
             (let* ((t0 (br-u8 glyph-stream)) (b2 (br-u8 glyph-stream)) (t2 (br-u8 glyph-stream)))
               (setf dx (tri-sign flag (+ (ash t0 4) (ash b2 -4)))
                     dy (tri-sign (ash flag -1) (+ (ash (logand b2 #x0f) 8) t2)))))
            (t
             (let* ((t0 (br-u8 glyph-stream)) (t1 (br-u8 glyph-stream))
                    (t2 (br-u8 glyph-stream)) (t3 (br-u8 glyph-stream)))
               (setf dx (tri-sign flag (+ (ash t0 8) t1))
                     dy (tri-sign (ash flag -1) (+ (ash t2 8) t3))))))
          (incf x dx) (incf y dy)
          (setf (aref xs i) x (aref ys i) y (aref oncv i) on-curve))))
    (values xs ys oncv)))

(defun woff2-emit-simple-glyph (w ncont n-points flag-stream glyph-stream
                                instruction-stream bbox-stream has-bbox)
  "Decode one transformed simple glyph and emit a standard glyf entry to W."
  ;; endPtsOfContours from nPointsStream (255UInt16 per contour, cumulative).
  (let ((endpts (make-array ncont)) (endpoint -1))
    (dotimes (c ncont)
      (incf endpoint (br-255ushort n-points))
      (setf (aref endpts c) endpoint))
    (let ((npts (1+ (aref endpts (1- ncont)))))
      (multiple-value-bind (xs ys oncv)
          (woff2-decode-triplets npts flag-stream glyph-stream)
        ;; instructionLength (255UInt16) from glyphStream; instrs from instructionStream.
        (let* ((ilen (br-255ushort glyph-stream))
               (instrs (br-bytes instruction-stream ilen)))
          ;; bbox: from bboxStream if flagged, else compute from points.
          (multiple-value-bind (xmin ymin xmax ymax)
              (if has-bbox
                  (values (br-s16 bbox-stream) (br-s16 bbox-stream)
                          (br-s16 bbox-stream) (br-s16 bbox-stream))
                  (woff2-compute-bbox xs ys npts))
            ;; ---- emit standard simple glyph ----
            (bw-s16 w ncont)
            (bw-s16 w xmin) (bw-s16 w ymin) (bw-s16 w xmax) (bw-s16 w ymax)
            (dotimes (c ncont) (bw-u16 w (aref endpts c)))
            (bw-u16 w ilen)
            (bw-bytes w instrs)
            ;; flags: emit one explicit flag per point (no repeat), on-curve bit 0.
            (dotimes (i npts)
              (bw-u8 w (if (aref oncv i) 1 0)))
            ;; x deltas as signed 16-bit words (flag bits 1/4 = 0 -> read s16).
            (let ((px 0))
              (dotimes (i npts) (bw-s16 w (- (aref xs i) px)) (setf px (aref xs i))))
            (let ((py 0))
              (dotimes (i npts) (bw-s16 w (- (aref ys i) py)) (setf py (aref ys i))))))))))

(defun woff2-compute-bbox (xs ys npts)
  (if (zerop npts)
      (values 0 0 0 0)
      (let ((xmin (aref xs 0)) (xmax (aref xs 0)) (ymin (aref ys 0)) (ymax (aref ys 0)))
        (dotimes (i npts)
          (setf xmin (min xmin (aref xs i)) xmax (max xmax (aref xs i))
                ymin (min ymin (aref ys i)) ymax (max ymax (aref ys i))))
        (values xmin ymin xmax ymax))))

;;; composite flag bits
(defconstant +arg-words+      #x0001)
(defconstant +we-have-scale+  #x0008)
(defconstant +more-components+ #x0020)
(defconstant +x-and-y-scale+  #x0040)
(defconstant +two-by-two+     #x0080)
(defconstant +have-instr+     #x0100)

(defun woff2-emit-composite-glyph (w composite-stream glyph-stream
                                   instruction-stream bbox-stream)
  "Copy composite component records verbatim from COMPOSITE-STREAM (they are in
   standard glyf component format), tracking lengths to know where they end.
   Composite glyphs ALWAYS have an explicit bbox in bboxStream."
  (let ((xmin (br-s16 bbox-stream)) (ymin (br-s16 bbox-stream))
        (xmax (br-s16 bbox-stream)) (ymax (br-s16 bbox-stream))
        (comp-w (make-bw 256)) (more t) (have-instr nil))
    (loop while more do
      (let* ((flags (br-u16 composite-stream))
             (gid (br-u16 composite-stream)))
        (bw-u16 comp-w flags) (bw-u16 comp-w gid)
        ;; args: 2 or 4 bytes
        (let ((nargs (if (logtest flags +arg-words+) 4 2)))
          (bw-bytes comp-w (br-bytes composite-stream nargs)))
        ;; transform: 0, 2, 4, or 8 bytes
        (let ((nx (cond ((logtest flags +two-by-two+) 8)
                        ((logtest flags +x-and-y-scale+) 4)
                        ((logtest flags +we-have-scale+) 2)
                        (t 0))))
          (when (plusp nx) (bw-bytes comp-w (br-bytes composite-stream nx))))
        (when (logtest flags +have-instr+) (setf have-instr t))
        (setf more (logtest flags +more-components+))))
    ;; emit standard composite glyph
    (bw-s16 w -1)
    (bw-s16 w xmin) (bw-s16 w ymin) (bw-s16 w xmax) (bw-s16 w ymax)
    (bw-bytes w (bw-octets comp-w))
    ;; instructions: only if WE_HAVE_INSTRUCTIONS was set on any component.
    (when have-instr
      (let* ((ilen (br-255ushort glyph-stream))
             (instrs (br-bytes instruction-stream ilen)))
        (bw-u16 w ilen)
        (bw-bytes w instrs)))))

;;; ===========================================================================
;;; Signature dispatch — exported helper used by open-font.
;;; ===========================================================================
(defun maybe-decompress-web-font (bytes)
  "If BYTES is a WOFF/WOFF2 font, decode to sfnt; otherwise return BYTES."
  (let ((d (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (if (>= (length d) 4)
        (let ((sig (u32 d 0)))
          (cond ((= sig #x774F4632) (woff2-decode d))   ; 'wOF2'
                ((= sig #x774F4646) (woff1-decode d))    ; 'wOFF'
                (t d)))
        d)))
