;;;; hint-weftdump — dump weft's HINTED glyph points (F26.6) for a diff against
;;;; the FreeType oracle (inspect/hint-oracle.py).  Also dumps the post-prep CVT
;;;; with mode "cvt".  Usage:
;;;;   sbcl --load inspect/hint-weftdump.lisp <font.ttf> <ppem> <chars>
;;;;   sbcl --load inspect/hint-weftdump.lisp <font.ttf> <ppem> cvt
(asdf:load-system :scribe)
(in-package :scribe)
(destructuring-bind (font-path ppem-s arg) (last sb-ext:*posix-argv* 3)
  (let* ((ppem (parse-integer ppem-s))
         (bytes (with-open-file (s font-path :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
         (font (open-font bytes)) (h (make-hinter font ppem)))
    (run-fpgm-prep h)
    (if (string= arg "cvt")
        (dotimes (i (length (hs-cvt h))) (format t "cvt[~a]=~a~%" i (aref (hs-cvt h) i)))
        (loop for ch across arg do
          (let ((cs (hint-glyph h (font-glyph-index font (char-code ch)))))
            (format t "PTS ~a" ch)
            (if (listp cs) (dolist (c cs) (loop for (x y on) in c do (format t " ~a ~a" (round (* x 64)) (round (* y 64)))))
                (format t " NA"))
            (terpri))))))
