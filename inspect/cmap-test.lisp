;;;; cmap-test.lisp — per-format cmap oracle gate.
;;;;
;;;; Loaded AFTER (asdf:load-system "scribe"). For unit "cmap-<fmt>", runs the
;;;; unit's (parse-cmap-<fmt> d off) on every corpus subtable of that format and
;;;; diffs the codepoint->gid map against inspect/vectors/cmap/<stem>.<pid>.<eid>.<fmt>.tsv.
;;;;
;;;;   ... --eval '(asdf:load-system "scribe")' --load inspect/cmap-test.lisp \
;;;;       --eval '(scribe.ctest:run "cmap-4")'
(defpackage #:scribe.ctest (:use #:cl) (:export #:run))
(in-package #:scribe.ctest)

(defparameter *here* (directory-namestring (or *load-truename* *load-pathname*)))

(defun read-font (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun split-dots (s)
  (loop with start = 0 for dot = (position #\. s :start start)
        collect (subseq s start (or dot (length s)))
        while dot do (setf start (1+ dot))))

(defun run (unit)
  (let* ((fmt (subseq unit (1+ (position #\- unit))))           ; "cmap-4" -> "4"
         (parser (find-symbol (format nil "PARSE-~:@(~a~)" unit) :scribe))
         (passed 0) (failed 0) (examples '()) (files 0))
    (unless (and parser (fboundp parser))
      (format t "~&~a: NO PARSER — FAIL~%" unit) (return-from run))
    (dolist (tsv (directory (merge-pathnames "vectors/cmap/*.tsv" *here*)))
      (let ((parts (split-dots (pathname-name tsv))))      ; (stem pid eid fmt)
        (when (and (= (length parts) 4) (string= (fourth parts) fmt))
          (incf files)
          (destructuring-bind (stem pid eid f) parts
            (let* ((font (scribe:open-font
                          (read-font (merge-pathnames (format nil "corpus/~a.ttf" stem) *here*))))
                   (sub (find-if (lambda (s) (and (= (first s) (parse-integer pid))
                                                  (= (second s) (parse-integer eid))
                                                  (= (third s) (parse-integer f))))
                                 (scribe::cmap-subtables font))))
              (if (null sub)
                  (progn (incf failed)
                         (when (< (length examples) 6)
                           (push (format nil "~a: subtable ~a/~a fmt ~a not found" stem pid eid f) examples)))
                  (let ((ht (funcall parser (scribe::font-data font) (fourth sub)))
                        (oracle-n 0))
                    (with-open-file (s tsv)
                      (loop for line = (read-line s nil) while line
                            for tab = (position #\Tab line)
                            when tab do
                              (incf oracle-n)
                              (let* ((cp (parse-integer line :end tab))
                                     (want (parse-integer line :start (1+ tab)))
                                     (got (gethash cp ht)))
                                (if (eql got want) (incf passed)
                                    (progn (incf failed)
                                           (when (< (length examples) 6)
                                             (push (format nil "~a fmt~a cp ~a: got ~a want ~a" stem f cp got want) examples)))))))
                    ;; extras: scribe mapped codepoints the oracle doesn't have
                    (let ((extra (- (hash-table-count ht) oracle-n)))
                      (when (plusp extra)
                        (incf failed extra)
                        (when (< (length examples) 6)
                          (push (format nil "~a fmt~a: ~a EXTRA mappings beyond oracle" stem f extra) examples)))))))))))
    (when (zerop files)
      (format t "~&~a: 0 subtables checked — FAIL (vacuous)~%" unit) (return-from run))
    (format t "~&~a: ~d passed, ~d failed  (~d subtables)~%" unit passed failed files)
    (dolist (e (nreverse examples)) (format t "    ~a~%" e))))
