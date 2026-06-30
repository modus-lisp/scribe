;;;; run-all.lisp — run every offline gate; exit nonzero if any fails.
;;;; Used by inspect/run-all.sh (CI). Needs only scribe + the vendored oracle
;;;; vectors + the redistributable corpus fonts — no Python/HarfBuzz/FreeType.
(defparameter *insp* (directory-namestring (or *load-truename* *load-pathname*)))
(dolist (f '("table-test" "cmap-test" "shape-test"))
  (load (merge-pathnames (format nil "~a.lisp" f) *insp*)))

(defun frac-eq (s marker)
  "T iff the 'A/B' immediately after MARKER in S has A = B."
  (let ((p (search marker s)))
    (when p
      (let* ((q (+ p (length marker)))
             (sl (position #\/ s :start q))
             (a (and sl (parse-integer s :start q :end sl :junk-allowed t)))
             (e (when sl (1+ sl))))
        (when sl
          (loop while (and (< e (length s)) (digit-char-p (char s e))) do (incf e))
          (let ((b (parse-integer s :start (1+ sl) :end e :junk-allowed t)))
            (and a b (= a b))))))))

(let ((fails 0))
  (flet ((cap (thunk) (with-output-to-string (*standard-output*) (funcall thunk)))
         (chk (label ok out)
           (format t "~&[~a] ~a~%" (if ok "PASS" "FAIL") label)
           (unless ok (incf fails) (format t "~a~%" out))))
    (dolist (u '("head" "hmtx" "os_2" "post" "name"))
      (let ((o (cap (lambda () (funcall (find-symbol "RUN" :scribe.test) u)))))
        (chk (format nil "table:~a" u)
             (and (search ", 0 failed" o) (not (search "vacuous" o)) (not (search "NO PARSER" o))) o)))
    (dolist (u '("cmap-6" "cmap-4" "cmap-12"))
      (let ((o (cap (lambda () (funcall (find-symbol "RUN" :scribe.ctest) u)))))
        (chk (format nil "cmap:~a" u) (search ", 0 failed" o) o)))
    (let ((o (cap (find-symbol "RUN-ALL" :scribe.stest))))
      (chk "shape:run-all (vs HarfBuzz)" (and (frac-eq o "shaping: ") (frac-eq o "match; ")) o))
    (let ((o (cap (find-symbol "RUN-FEATURES" :scribe.stest))))
      (chk "shape:features (vs HarfBuzz)" (frac-eq o "full ") o)))
  (format t "~%=== ~a ===~%"
          (if (zerop fails) "ALL GATES PASS" (format nil "~d GATE(S) FAILED" fails)))
  (uiop:quit (if (zerop fails) 0 1)))
