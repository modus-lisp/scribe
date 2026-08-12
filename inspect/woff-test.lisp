;;;; woff-test.lisp — WOFF1 round-trip gate.
;;;;
;;;; Loaded AFTER (asdf:load-system "scribe").  Builds a real WOFF1 from each
;;;; corpus font — per-table zlib exactly as the format specifies — then decodes
;;;; it back with WOFF1-DECODE and checks every table is byte-identical to the
;;;; original sfnt's.
;;;;
;;;; This is the one place the library depends on cram's inflate, and it had no
;;;; gate: the WOFF1 path was only ever exercised by hand.  A wrong inflate here
;;;; is silent — you get a font that parses and renders subtly wrong glyphs.
;;;;
;;;;   ... --eval '(asdf:load-system "scribe")' --load inspect/woff-test.lisp \
;;;;       --eval '(scribe.wtest:run)'
(defpackage #:scribe.wtest (:use #:cl) (:export #:run))
(in-package #:scribe.wtest)

(defparameter *here* (directory-namestring (or *load-truename* *load-pathname*)))

(defun read-font (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun u8v (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun be32 (v o x) (setf (aref v o) (ldb (byte 8 24) x) (aref v (+ o 1)) (ldb (byte 8 16) x)
                          (aref v (+ o 2)) (ldb (byte 8 8) x) (aref v (+ o 3)) (ldb (byte 8 0) x)))
(defun be16 (v o x) (setf (aref v o) (ldb (byte 8 8) x) (aref v (+ o 1)) (ldb (byte 8 0) x)))

(defun sfnt-tables (d)
  "((tag . bytes) ...) from an sfnt, in directory order."
  (let ((n (logior (ash (aref d 4) 8) (aref d 5))) (out '()))
    (dotimes (i n (nreverse out))
      (let* ((rec (+ 12 (* i 16)))
             (tag (map 'string #'code-char (subseq d rec (+ rec 4))))
             (off (logior (ash (aref d (+ rec 8)) 24) (ash (aref d (+ rec 9)) 16)
                          (ash (aref d (+ rec 10)) 8) (aref d (+ rec 11))))
             (len (logior (ash (aref d (+ rec 12)) 24) (ash (aref d (+ rec 13)) 16)
                          (ash (aref d (+ rec 14)) 8) (aref d (+ rec 15)))))
        (push (cons tag (subseq d off (min (length d) (+ off len)))) out)))))

(defun build-woff1 (sfnt)
  "Wrap SFNT as a WOFF1 (per-table zlib, kept only when it actually shrinks —
   which is what makes both branches of WOFF1-DECODE's compressed test run)."
  (let* ((tables (sfnt-tables sfnt))
         (n (length tables))
         (flavor (logior (ash (aref sfnt 0) 24) (ash (aref sfnt 1) 16)
                         (ash (aref sfnt 2) 8) (aref sfnt 3)))
         (dir-end (+ 44 (* n 20)))
         (bodies '()) (offset dir-end) (recs '()))
    (dolist (tb tables)
      (let* ((orig (cdr tb))
             (z (cram:zlib-compress orig))
             (use (if (< (length z) (length orig)) z orig))
             (pad (mod (- 4 (mod (length use) 4)) 4)))
        (push (list (car tb) offset (length use) (length orig)) recs)
        (push use bodies) (push (u8v pad) bodies)
        (incf offset (+ (length use) pad))))
    (let ((out (u8v offset)))
      (be32 out 0 #x774F4646)                     ; 'wOFF'
      (be32 out 4 flavor)
      (be32 out 8 offset)                         ; length
      (be16 out 12 n) (be16 out 14 0)             ; numTables, reserved
      (be32 out 16 (length sfnt))                 ; totalSfntSize
      (be16 out 20 1) (be16 out 22 0)             ; version
      ;; meta/priv offsets+lengths stay zero (bytes 24..43)
      (loop for r in (nreverse recs) for i from 0
            do (let ((rec (+ 44 (* i 20))))
                 (loop for k below 4 do (setf (aref out (+ rec k)) (char-code (char (first r) k))))
                 (be32 out (+ rec 4) (second r))   ; offset
                 (be32 out (+ rec 8) (third r))    ; compLength
                 (be32 out (+ rec 12) (fourth r))  ; origLength
                 (be32 out (+ rec 16) 0)))         ; origChecksum (unchecked)
      (let ((p dir-end))
        (dolist (b (nreverse bodies))
          (replace out b :start1 p) (incf p (length b))))
      out)))

(defun run ()
  (let ((passed 0) (failed 0) (fonts 0) (compressed 0) (examples '()))
    (dolist (f (append (directory (merge-pathnames "corpus/*.ttf" *here*))
                       (directory (merge-pathnames "corpus/*.otf" *here*))))
      (incf fonts)
      (let* ((sfnt (read-font f))
             (want (sfnt-tables sfnt))
             (woff (build-woff1 sfnt))
             (back (handler-case (funcall (find-symbol "WOFF1-DECODE" :scribe) woff)
                     (error (e) (incf failed)
                       (push (format nil "~a: decode error: ~a" (pathname-name f) e) examples)
                       nil))))
        ;; count the tables that really did travel compressed, so a build where
        ;; nothing compressed can't pass vacuously
        (dolist (tb want)
          (when (< (length (cram:zlib-compress (cdr tb))) (length (cdr tb))) (incf compressed)))
        (when back
          (let ((got (sfnt-tables back)))
            (dolist (tb want)
              (let ((g (assoc (car tb) got :test #'string=)))
                (cond ((null g) (incf failed)
                       (push (format nil "~a: table ~a missing after decode"
                                     (pathname-name f) (car tb)) examples))
                      ((not (equalp (cdr g) (cdr tb))) (incf failed)
                       (push (format nil "~a: table ~a differs (~d vs ~d bytes)"
                                     (pathname-name f) (car tb)
                                     (length (cdr g)) (length (cdr tb))) examples))
                      (t (incf passed)))))))))
    (when (or (zerop fonts) (zerop compressed))
      (format t "~&woff1: ~d fonts, ~d compressed tables — FAIL (vacuous)~%" fonts compressed)
      (return-from run))
    (format t "~&woff1: ~d passed, ~d failed  (~d fonts, ~d tables compressed)~%"
            passed failed fonts compressed)
    (dolist (e (nreverse examples)) (format t "    ~a~%" e))))
