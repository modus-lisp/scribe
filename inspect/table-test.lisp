;;;; table-test.lisp — per-table oracle gate for the W1 swarm.
;;;;
;;;; Loaded AFTER (asdf:load-system "scribe"). Diffs (parse-<unit> font) against
;;;; the vendored ground truth inspect/vectors/tables/<font>.tsv (from
;;;; ttx-oracle.py). Non-vacuous: zero fields checked => FAIL (SWARM.md inv. 3).
;;;;
;;;;   sbcl ... --eval '(asdf:load-system "scribe")' \
;;;;       --load inspect/table-test.lisp --eval '(scribe.test:run "head")'
(defpackage #:scribe.test (:use #:cl) (:export #:run))
(in-package #:scribe.test)

(defparameter *here* (directory-namestring (or *load-truename* *load-pathname*)))

(defun read-font (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun read-tsv (path)
  "List of (field-string . value-string)."
  (with-open-file (s path)
    (loop for line = (read-line s nil) while line
          for tab = (position #\Tab line)
          when tab collect (cons (subseq line 0 tab) (subseq line (1+ tab))))))

(defun as-number (str)
  (let ((*read-eval* nil))
    (ignore-errors (let ((v (read-from-string str nil nil))) (and (numberp v) v)))))

(defun field= (scribe-val oracle-str)
  (let ((sn (and (numberp scribe-val) (as-number oracle-str))))
    (if sn
        (< (abs (- scribe-val sn)) 1d-4)
        (string= (princ-to-string scribe-val) oracle-str))))

(defun run (unit)
  "Gate for one table UNIT (e.g. \"head\", \"OS_2\"). Prints N passed, M failed."
  (let* ((parser (find-symbol (format nil "PARSE-~:@(~a~)" unit) :scribe))
         (prefix (concatenate 'string unit "."))
         (plen (length prefix))
         (passed 0) (failed 0) (examples '()))
    (unless (and parser (fboundp parser))
      (format t "~&~a: NO PARSER (scribe::parse-~a undefined) — FAIL~%" unit (string-downcase unit))
      (return-from run))
    (dolist (tsv (directory (merge-pathnames "vectors/tables/*.tsv" *here*)))
      (let* ((stem (pathname-name tsv))
             (font-path (merge-pathnames (format nil "corpus/~a.ttf" stem) *here*)))
        (when (probe-file font-path)
          (let* ((font (scribe:open-font (read-font font-path)))
                 (got (funcall parser font)))
            (dolist (row (read-tsv tsv))
              (when (and (>= (length (car row)) plen)
                         (string= prefix (car row) :end2 plen))
                (let* ((field (subseq (car row) plen))
                       (cell (assoc field got :test #'string=)))
                  (cond
                    ((null cell)
                     (incf failed)
                     (when (< (length examples) 6)
                       (push (format nil "~a ~a: MISSING (oracle=~a)" stem field (cdr row)) examples)))
                    ((field= (cdr cell) (cdr row)) (incf passed))
                    (t (incf failed)
                       (when (< (length examples) 6)
                         (push (format nil "~a ~a: got ~a want ~a" stem field (cdr cell) (cdr row)) examples)))))))))))
    (when (zerop (+ passed failed))
      (format t "~&~a: 0 fields checked — FAIL (vacuous)~%" unit)
      (return-from run))
    (format t "~&~a: ~d passed, ~d failed~%" unit passed failed)
    (dolist (e (nreverse examples)) (format t "    ~a~%" e))))
