;;;; deflate.lisp — pure Common Lisp DEFLATE (RFC 1951) inflater + zlib/gzip wrappers.
;;;;
;;;; Clean-room. No FFI, no external libraries (no chipz). Reusable.
;;;;
;;;; Canonical Huffman decode follows Mark Adler's "puff" approach: a code is
;;;; described by per-length code *counts* and a *symbols* array sorted by
;;;; (length, symbol). Decoding walks bit-by-bit, accumulating the code value
;;;; and subtracting the codes of each length until the value falls inside the
;;;; range for the current length.
;;;;
;;;; Bits are read LSB-first within each byte (DEFLATE convention); Huffman code
;;;; bits are consumed MSB-first into the running code value.
;;;;
;;;; Public entry points (all return a (simple-array (unsigned-byte 8) (*))):
;;;;   (inflate bytes &key (start 0) (end nil) (expected-size nil))  ; raw deflate
;;;;   (zlib-decompress bytes &key (start 0) (end nil))
;;;;   (gzip-decompress bytes &key (start 0) (end nil))

(defpackage #:deflate
  (:use #:cl)
  (:export #:inflate #:zlib-decompress #:gzip-decompress #:+ub8+))
(in-package #:deflate)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(deftype ub8 () '(unsigned-byte 8))
(deftype ub8v () '(simple-array (unsigned-byte 8) (*)))
(defmacro +ub8+ () ''(unsigned-byte 8))

;;; ---------------------------------------------------------------------------
;;; Bit reader state — a struct holding the source vector, byte cursor, and a
;;; small bit accumulator. LSB-first.
;;; ---------------------------------------------------------------------------

(defstruct (bitr (:constructor make-bitr (src pos end)))
  (src (make-array 0 :element-type 'ub8) :type ub8v)
  (pos 0 :type fixnum)            ; next byte to consume
  (end 0 :type fixnum)            ; one past last usable byte
  (bitbuf 0 :type (unsigned-byte 32))
  (bitcnt 0 :type (integer 0 32)))

(declaim (inline getbit getbits))

(defun getbit (br)
  "Read one bit, LSB-first."
  (declare (type bitr br))
  (when (zerop (bitr-bitcnt br))
    (let ((p (bitr-pos br)))
      (when (>= p (bitr-end br))
        (error "deflate: out of input"))
      (setf (bitr-bitbuf br) (aref (bitr-src br) p)
            (bitr-pos br) (the fixnum (1+ p))
            (bitr-bitcnt br) 8)))
  (let ((b (logand (bitr-bitbuf br) 1)))
    (setf (bitr-bitbuf br) (ash (bitr-bitbuf br) -1)
          (bitr-bitcnt br) (the fixnum (1- (bitr-bitcnt br))))
    b))

(defun getbits (br n)
  "Read N bits (0..16), LSB-first, low bit first."
  (declare (type bitr br) (type (integer 0 16) n))
  ;; refill so we have at least N bits
  (loop while (< (bitr-bitcnt br) n) do
    (let ((p (bitr-pos br)))
      (when (>= p (bitr-end br))
        (error "deflate: out of input"))
      (setf (bitr-bitbuf br)
            (logior (bitr-bitbuf br)
                    (the (unsigned-byte 32)
                         (ash (aref (bitr-src br) p) (bitr-bitcnt br))))
            (bitr-pos br) (the fixnum (1+ p))
            (bitr-bitcnt br) (the fixnum (+ (bitr-bitcnt br) 8)))))
  (let ((v (logand (bitr-bitbuf br) (1- (ash 1 n)))))
    (setf (bitr-bitbuf br) (ash (bitr-bitbuf br) (- n))
          (bitr-bitcnt br) (the fixnum (- (bitr-bitcnt br) n)))
    v))

(defun align-byte (br)
  "Discard remaining bits in the current partial byte."
  (declare (type bitr br))
  (setf (bitr-bitbuf br) 0
        (bitr-bitcnt br) 0))

;;; ---------------------------------------------------------------------------
;;; Output buffer — growable typed ub8 vector with a fill pointer we manage by
;;; hand (faster than a real adjustable array's bounds checks in the hot loop).
;;; ---------------------------------------------------------------------------

(defstruct (outbuf (:constructor %make-outbuf (data)))
  (data (make-array 0 :element-type 'ub8) :type ub8v)
  (len 0 :type fixnum))

(defun make-outbuf (&optional (cap 65536))
  (declare (type fixnum cap))
  (%make-outbuf (make-array cap :element-type 'ub8)))

(declaim (inline ob-ensure))
(defun ob-ensure (ob extra)
  (declare (type outbuf ob) (type fixnum extra))
  (let* ((d (outbuf-data ob))
         (need (the fixnum (+ (outbuf-len ob) extra)))
         (cap (length d)))
    (declare (type ub8v d) (type fixnum need cap))
    (when (> need cap)
      (let ((new (the fixnum (max need (the fixnum (* cap 2))))))
        (let ((nd (make-array new :element-type 'ub8)))
          (replace nd d :end1 (outbuf-len ob))
          (setf (outbuf-data ob) nd))))))

(declaim (inline ob-push))
(defun ob-push (ob byte)
  (declare (type outbuf ob) (type ub8 byte))
  (ob-ensure ob 1)
  (let ((l (outbuf-len ob)))
    (setf (aref (outbuf-data ob) l) byte
          (outbuf-len ob) (the fixnum (1+ l)))))

(defun ob-finish (ob)
  (declare (type outbuf ob))
  (subseq (outbuf-data ob) 0 (outbuf-len ob)))

;;; ---------------------------------------------------------------------------
;;; Huffman tables (puff-style): counts[1..maxbits], symbols[] sorted by length.
;;; ---------------------------------------------------------------------------

(defstruct (huff (:constructor %make-huff (counts symbols)))
  (counts  (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (symbols (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

(defconstant +maxbits+ 15)

(defun build-huff (lengths n)
  "Build a huff from the first N code lengths in LENGTHS (each 0..15)."
  (declare (type (simple-array fixnum (*)) lengths) (type fixnum n))
  (let ((counts (make-array (1+ +maxbits+) :element-type 'fixnum :initial-element 0))
        (symbols (make-array n :element-type 'fixnum :initial-element 0)))
    ;; count codes of each length
    (dotimes (s n)
      (incf (aref counts (aref lengths s))))
    ;; offsets[len] = starting index in symbols for that length.
    ;; Length-0 (absent) symbols are NOT stored, so offs[1] = 0 and each
    ;; subsequent length starts after the previous length's codes (puff).
    (let ((offs (make-array (1+ +maxbits+) :element-type 'fixnum :initial-element 0)))
      (setf (aref offs 1) 0)
      (loop for len from 2 to +maxbits+ do
        (setf (aref offs len) (+ (aref offs (1- len)) (aref counts (1- len)))))
      (dotimes (s n)
        (let ((len (aref lengths s)))
          (when (plusp len)
            (setf (aref symbols (aref offs len)) s)
            (incf (aref offs len))))))
    (%make-huff counts symbols)))

(declaim (inline decode-sym))
(defun decode-sym (br h)
  "Decode one symbol from BR using huff H (puff canonical walk)."
  (declare (type bitr br) (type huff h))
  (let ((counts (huff-counts h))
        (symbols (huff-symbols h))
        (code 0) (first 0) (index 0))
    (declare (type (simple-array fixnum (*)) counts symbols)
             (type fixnum code first index))
    (loop for len from 1 to +maxbits+ do
      (setf code (logior (ash code 1) (getbit br)))
      (let ((count (aref counts len)))
        (declare (type fixnum count))
        (when (< (- code first) count)
          (return-from decode-sym (aref symbols (+ index (- code first)))))
        (incf index count)
        (setf first (ash (+ first count) 1))))
    (error "deflate: bad symbol (ran off code table)")))

;;; ---------------------------------------------------------------------------
;;; Length / distance tables (RFC 1951 §3.2.5).
;;; ---------------------------------------------------------------------------

(declaim (type (simple-array fixnum (*)) +len-base+ +len-extra+ +dist-base+ +dist-extra+))
(defparameter +len-base+
  (make-array 29 :element-type 'fixnum
    :initial-contents '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
                        35 43 51 59 67 83 99 115 131 163 195 227 258)))
(defparameter +len-extra+
  (make-array 29 :element-type 'fixnum
    :initial-contents '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2
                        3 3 3 3 4 4 4 4 5 5 5 5 0)))
(defparameter +dist-base+
  (make-array 30 :element-type 'fixnum
    :initial-contents '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
                        257 385 513 769 1025 1537 2049 3073 4097 6145
                        8193 12289 16385 24577)))
(defparameter +dist-extra+
  (make-array 30 :element-type 'fixnum
    :initial-contents '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6
                        7 7 8 8 9 9 10 10 11 11 12 12 13 13)))

(defconstant +clc-order+
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

;;; Fixed Huffman tables (built once).
(defparameter *fixed-lit* nil)
(defparameter *fixed-dist* nil)

(defun ensure-fixed ()
  (unless *fixed-lit*
    (let ((ll (make-array 288 :element-type 'fixnum)))
      (loop for i from 0 to 143 do (setf (aref ll i) 8))
      (loop for i from 144 to 255 do (setf (aref ll i) 9))
      (loop for i from 256 to 279 do (setf (aref ll i) 7))
      (loop for i from 280 to 287 do (setf (aref ll i) 8))
      (setf *fixed-lit* (build-huff ll 288)))
    (let ((dl (make-array 30 :element-type 'fixnum :initial-element 5)))
      (setf *fixed-dist* (build-huff dl 30)))))

;;; ---------------------------------------------------------------------------
;;; Block decoders.
;;; ---------------------------------------------------------------------------

(defun inflate-block-huff (br ob lit dist)
  "Decode a compressed block body given literal/length and distance huffs."
  (declare (type bitr br) (type outbuf ob) (type huff lit dist))
  (loop
    (let ((sym (decode-sym br lit)))
      (declare (type fixnum sym))
      (cond
        ((< sym 256) (ob-push ob sym))
        ((= sym 256) (return))           ; end of block
        (t
         (let* ((li (- sym 257)))
           (declare (type fixnum li))
           (when (> li 28) (error "deflate: bad length symbol ~d" sym))
           (let* ((length (+ (aref +len-base+ li)
                             (getbits br (aref +len-extra+ li))))
                  (dsym (decode-sym br dist)))
             (declare (type fixnum length dsym))
             (when (> dsym 29) (error "deflate: bad distance symbol ~d" dsym))
             (let ((distance (+ (aref +dist-base+ dsym)
                                (getbits br (aref +dist-extra+ dsym)))))
               (declare (type fixnum distance length))
               (ob-ensure ob length)
               (let* ((d (outbuf-data ob))
                      (l (outbuf-len ob))
                      (src (- l distance)))
                 (declare (type ub8v d) (type fixnum l src))
                 (when (< src 0) (error "deflate: distance too far back"))
                 ;; byte-by-byte copy (handles overlap where distance < length)
                 (dotimes (k length)
                   (setf (aref d (+ l k)) (aref d (+ src k))))
                 (setf (outbuf-len ob) (the fixnum (+ l length))))))))))))

(defun inflate-dynamic (br ob)
  (declare (type bitr br) (type outbuf ob))
  (let* ((hlit  (+ 257 (getbits br 5)))
         (hdist (+ 1   (getbits br 5)))
         (hclen (+ 4   (getbits br 4)))
         (cl-lengths (make-array 19 :element-type 'fixnum :initial-element 0)))
    (declare (type fixnum hlit hdist hclen))
    (dotimes (i hclen)
      (setf (aref cl-lengths (aref +clc-order+ i)) (getbits br 3)))
    (let* ((cl-huff (build-huff cl-lengths 19))
           (total (+ hlit hdist))
           (lengths (make-array total :element-type 'fixnum :initial-element 0))
           (i 0))
      (declare (type fixnum total i))
      (loop while (< i total) do
        (let ((sym (decode-sym br cl-huff)))
          (declare (type fixnum sym))
          (cond
            ((< sym 16) (setf (aref lengths i) sym) (incf i))
            ((= sym 16)
             (when (zerop i) (error "deflate: repeat with no previous length"))
             (let ((prev (aref lengths (1- i)))
                   (rep (+ 3 (getbits br 2))))
               (dotimes (k rep) (setf (aref lengths i) prev) (incf i))))
            ((= sym 17)
             (let ((rep (+ 3 (getbits br 3))))
               (dotimes (k rep) (setf (aref lengths i) 0) (incf i))))
            ((= sym 18)
             (let ((rep (+ 11 (getbits br 7))))
               (dotimes (k rep) (setf (aref lengths i) 0) (incf i))))
            (t (error "deflate: bad code-length symbol ~d" sym)))))
      (when (> i total) (error "deflate: code lengths overflow"))
      (let ((lit (build-huff lengths hlit))
            ;; distance lengths are the tail of the same array
            (dist (build-huff (let ((da (make-array hdist :element-type 'fixnum)))
                                (dotimes (k hdist) (setf (aref da k) (aref lengths (+ hlit k))))
                                da)
                              hdist)))
        (inflate-block-huff br ob lit dist)))))

(defun inflate-stored (br ob)
  (declare (type bitr br) (type outbuf ob))
  (align-byte br)
  (let ((p (bitr-pos br)) (src (bitr-src br)) (end (bitr-end br)))
    (declare (type fixnum p end) (type ub8v src))
    (when (> (+ p 4) end) (error "deflate: truncated stored header"))
    (let ((len  (logior (aref src p) (ash (aref src (+ p 1)) 8)))
          (nlen (logior (aref src (+ p 2)) (ash (aref src (+ p 3)) 8))))
      (declare (type fixnum len nlen))
      (unless (= len (logand (lognot nlen) #xffff))
        (error "deflate: stored LEN/NLEN mismatch"))
      (incf p 4)
      (when (> (+ p len) end) (error "deflate: truncated stored data"))
      (ob-ensure ob len)
      (let ((d (outbuf-data ob)) (l (outbuf-len ob)))
        (replace d src :start1 l :start2 p :end2 (+ p len))
        (setf (outbuf-len ob) (+ l len)))
      (setf (bitr-pos br) (+ p len)))))

;;; ---------------------------------------------------------------------------
;;; Top-level inflate.
;;; ---------------------------------------------------------------------------

(defun inflate (bytes &key (start 0) (end nil) (expected-size nil))
  "Inflate a raw DEFLATE stream from BYTES[START..END). Returns a fresh ub8v."
  (let* ((src (if (typep bytes 'ub8v) bytes
                  (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
         (e (or end (length src)))
         (br (make-bitr src start e))
         (ob (make-outbuf (if expected-size (max 65536 expected-size) 65536))))
    (declare (type ub8v src))
    (ensure-fixed)
    (loop
      (let ((bfinal (getbit br))
            (btype (getbits br 2)))
        (ecase btype
          (0 (inflate-stored br ob))
          (1 (inflate-block-huff br ob *fixed-lit* *fixed-dist*))
          (2 (inflate-dynamic br ob))
          (3 (error "deflate: reserved BTYPE 3")))
        (when (= bfinal 1) (return))))
    (ob-finish ob)))

;;; ---------------------------------------------------------------------------
;;; Wrappers.
;;; ---------------------------------------------------------------------------

(defun zlib-decompress (bytes &key (start 0) (end nil))
  "RFC 1950: 2-byte CMF/FLG header, deflate body, trailing adler32 (ignored)."
  (let* ((src (if (typep bytes 'ub8v) bytes
                  (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
         (e (or end (length src))))
    (when (< (- e start) 2) (error "zlib: too short"))
    (let* ((cmf (aref src start))
           (flg (aref src (1+ start)))
           (cm (logand cmf #x0f))
           (off (+ start 2)))
      (unless (= cm 8) (error "zlib: unexpected compression method ~d" cm))
      (when (logbitp 5 flg)            ; FDICT present
        (incf off 4))
      (inflate src :start off :end e))))

(defun gzip-decompress (bytes &key (start 0) (end nil))
  "RFC 1952: parse gzip header (with optional FEXTRA/FNAME/FCOMMENT/FHCRC),
   inflate body, ignore trailing crc32+isize."
  (let* ((src (if (typep bytes 'ub8v) bytes
                  (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
         (e (or end (length src)))
         (p start))
    (declare (type ub8v src) (type fixnum p e))
    (when (< (- e start) 10) (error "gzip: too short"))
    (unless (and (= (aref src p) #x1f) (= (aref src (1+ p)) #x8b))
      (error "gzip: bad magic"))
    (let ((cm (aref src (+ p 2)))
          (flg (aref src (+ p 3))))
      (unless (= cm 8) (error "gzip: unexpected method ~d" cm))
      (incf p 10)                      ; magic(2)+cm(1)+flg(1)+mtime(4)+xfl(1)+os(1)
      (when (logbitp 2 flg)            ; FEXTRA
        (let ((xlen (logior (aref src p) (ash (aref src (1+ p)) 8))))
          (incf p (+ 2 xlen))))
      (when (logbitp 3 flg)            ; FNAME (NUL-terminated)
        (loop until (zerop (aref src p)) do (incf p)) (incf p))
      (when (logbitp 4 flg)            ; FCOMMENT
        (loop until (zerop (aref src p)) do (incf p)) (incf p))
      (when (logbitp 1 flg)            ; FHCRC
        (incf p 2))
      (inflate src :start p :end e))))
