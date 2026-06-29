;;;; blend.lisp — gamma-correct, linear-light coverage compositing.
;;;;
;;;; This is step zero of first-class text rendering and the single most common
;;;; thing naive rasterizers get wrong: glyph coverage (an alpha mask in [0,1])
;;;; must be composited in LINEAR light, not in sRGB.  Blending in sRGB makes
;;;; partial-coverage edges — i.e. every anti-aliased stroke — render too dark,
;;;; so thin strokes look heavy and muddy and stems lose contrast.  Doing it in
;;;; linear light is what makes text look "real".
;;;;
;;;; Everything here is pure Common Lisp, no dependencies — scribe owns its own
;;;; canvas + PNG so its tests need nothing external.
(in-package #:scribe)

;;; ---------------------------------------------------------------------------
;;; sRGB transfer function (IEC 61966-2-1)
;;; ---------------------------------------------------------------------------

(declaim (type (simple-array double-float (256)) *srgb->linear*))
(defparameter *srgb->linear*
  (let ((a (make-array 256 :element-type 'double-float)))
    (dotimes (i 256 a)
      (let ((s (/ (float i 1d0) 255d0)))
        (setf (aref a i)
              (if (<= s 0.04045d0)
                  (/ s 12.92d0)
                  (expt (/ (+ s 0.055d0) 1.055d0) 2.4d0))))))
  "LUT: 8-bit sRGB channel -> linear-light [0,1] (double).")

(declaim (inline srgb->linear linear->srgb))
(defun srgb->linear (c8) (aref *srgb->linear* c8))

(defun linear->srgb (l)
  "Linear-light [0,1] -> 8-bit sRGB channel, clamped+rounded."
  (declare (type double-float l))
  (let* ((l (max 0d0 (min 1d0 l)))
         (s (if (<= l 0.0031308d0)
                (* l 12.92d0)
                (- (* 1.055d0 (expt l (/ 1d0 2.4d0))) 0.055d0))))
    (the (unsigned-byte 8) (min 255 (max 0 (round (* s 255d0)))))))

;;; A 4096-entry reverse LUT keeps the hot compositing path off `expt`.
(declaim (type (simple-array (unsigned-byte 8) (4097)) *linear->srgb*))
(defparameter *linear->srgb*
  (let ((a (make-array 4097 :element-type '(unsigned-byte 8))))
    (dotimes (i 4097 a)
      (setf (aref a i) (linear->srgb (/ (float i 1d0) 4096d0))))))

(declaim (inline lin->srgb8))
(defun lin->srgb8 (l)
  (declare (type double-float l))
  (aref *linear->srgb* (the (integer 0 4096)
                            (min 4096 (max 0 (round (* l 4096d0)))))))

;;; ---------------------------------------------------------------------------
;;; Canvas
;;; ---------------------------------------------------------------------------

(defstruct (canvas (:constructor %make-canvas))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixels nil :type (simple-array (unsigned-byte 8) (*))))  ; row-major RGB8

(defun make-canvas (w h &optional (bg '(255 255 255)))
  (let ((px (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length px) by 3
          do (setf (aref px i) (first bg)
                   (aref px (+ i 1)) (second bg)
                   (aref px (+ i 2)) (third bg)))
    (%make-canvas :width w :height h :pixels px)))

;;; ---------------------------------------------------------------------------
;;; Gamma-correct coverage compositing
;;; ---------------------------------------------------------------------------

(declaim (inline blend-coverage))
(defun blend-coverage (cv x y coverage fg)
  "Composite FG (list R G B, 8-bit) over pixel (X,Y) at COVERAGE in [0,1],
   blending in linear light.  COVERAGE 1 = solid FG, 0 = untouched."
  (declare (type canvas cv) (type fixnum x y) (type double-float coverage))
  (when (and (>= x 0) (>= y 0) (< x (canvas-width cv)) (< y (canvas-height cv))
             (> coverage 0d0))
    (let* ((px (canvas-pixels cv))
           (i (* 3 (+ (* y (canvas-width cv)) x)))
           (a (min 1d0 coverage)) (ia (- 1d0 a))
           (fr (srgb->linear (first fg)))
           (fg* (srgb->linear (second fg)))
           (fb (srgb->linear (third fg))))
      (setf (aref px i)       (lin->srgb8 (+ (* ia (srgb->linear (aref px i)))       (* a fr)))
            (aref px (+ i 1)) (lin->srgb8 (+ (* ia (srgb->linear (aref px (+ i 1)))) (* a fg*)))
            (aref px (+ i 2)) (lin->srgb8 (+ (* ia (srgb->linear (aref px (+ i 2)))) (* a fb))))))
  (values))

(defun fill-coverage-span (cv x y coverages fg &optional (n (length coverages)))
  "Blend N pixels starting at (X,Y) using per-pixel COVERAGES (double-float vector)."
  (dotimes (k n) (blend-coverage cv (+ x k) y (aref coverages k) fg)))

;;; ---------------------------------------------------------------------------
;;; Minimal PNG writer (truecolor RGB8, single stored/uncompressed deflate)
;;; ---------------------------------------------------------------------------

(defun %u32be (v) (list (ldb (byte 8 24) v) (ldb (byte 8 16) v)
                        (ldb (byte 8 8) v) (ldb (byte 8 0) v)))

(defun %crc32 (bytes &optional (start 0) (end (length bytes)))
  (let ((crc #xffffffff))
    (loop for i from start below end do
      (setf crc (logxor crc (aref bytes i)))
      (dotimes (_ 8)
        (setf crc (if (logbitp 0 crc)
                      (logxor #xedb88320 (ash crc -1))
                      (ash crc -1)))))
    (logxor crc #xffffffff)))

(defun %adler32 (bytes)
  (let ((a 1) (b 0))
    (loop for x across bytes do
      (setf a (mod (+ a x) 65521) b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun %chunk (out type data)
  "Write a PNG chunk: len, type(4 ascii), data, crc(type+data)."
  (let ((typed (concatenate '(vector (unsigned-byte 8))
                            (map 'vector #'char-code type) data)))
    (dolist (x (%u32be (length data))) (vector-push-extend x out))
    (loop for x across typed do (vector-push-extend x out))
    (dolist (x (%u32be (%crc32 typed))) (vector-push-extend x out))))

(defun write-png (cv path)
  "Write CV to PATH as a truecolor PNG (zlib stored blocks — no compressor)."
  (let* ((w (canvas-width cv)) (h (canvas-height cv)) (px (canvas-pixels cv))
         ;; raw = filter-byte(0) + RGB row, per scanline
         (raw (make-array (* h (+ 1 (* w 3))) :element-type '(unsigned-byte 8)))
         (ri 0))
    (dotimes (y h)
      (setf (aref raw ri) 0) (incf ri)
      (dotimes (x (* w 3)) (setf (aref raw ri) (aref px (+ (* y w 3) x))) (incf ri)))
    ;; zlib stream: 2-byte header + stored deflate blocks + adler32
    (let ((z (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (vector-push-extend #x78 z) (vector-push-extend #x01 z)  ; CMF/FLG (no compression)
      (let ((pos 0) (len (length raw)))
        (loop while (< pos len) do
          (let* ((blk (min 65535 (- len pos))) (final (if (>= (+ pos blk) len) 1 0)))
            (vector-push-extend final z)
            (vector-push-extend (ldb (byte 8 0) blk) z)
            (vector-push-extend (ldb (byte 8 8) blk) z)
            (vector-push-extend (logxor #xff (ldb (byte 8 0) blk)) z)
            (vector-push-extend (logxor #xff (ldb (byte 8 8) blk)) z)
            (loop for i from pos below (+ pos blk) do (vector-push-extend (aref raw i) z))
            (incf pos blk))))
      (dolist (x (%u32be (%adler32 raw))) (vector-push-extend x z))
      (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (dolist (b '(137 80 78 71 13 10 26 10)) (vector-push-extend b out)) ; signature
        (%chunk out "IHDR" (coerce (append (%u32be w) (%u32be h)
                                           (list 8 2 0 0 0)) ; depth8 colortype2(RGB)
                                   '(vector (unsigned-byte 8))))
        (%chunk out "IDAT" (coerce z '(vector (unsigned-byte 8))))
        (%chunk out "IEND" #())
        (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create)
          (write-sequence out s)))
      path)))
