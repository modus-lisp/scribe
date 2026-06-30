;;;; src/tables/name.lisp — (parse-name font) -> alist
;;;; (field-string . value) matching the "name." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "name") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-name (font)
  (let* ((d (font-data font))
         (o (req-table font "name"))
         (count (u16 d (+ o 2)))
         (string-offset (u16 d (+ o 4)))
         (records-start (+ o 6))
         (result '()))
    (flet ((decode (platform-id encoding-id language-id name-id)
             (loop for i from 0 below count
                   for rec-start = (+ records-start (* i 12))
                   when (and (= (u16 d rec-start) platform-id)
                             (= (u16 d (+ rec-start 2)) encoding-id)
                             (= (u16 d (+ rec-start 4)) language-id)
                             (= (u16 d (+ rec-start 6)) name-id))
                   do (let* ((len (u16 d (+ rec-start 8)))
                             (off (u16 d (+ rec-start 10)))
                             (abs-off (+ o string-offset off)))
                        (return
                          (if (= platform-id 3)
                              ;; UTF-16BE: read each 2-byte pair as a codepoint
                              (let* ((n (floor len 2))
                                     (chars (make-array n :element-type 'character)))
                                (dotimes (j n)
                                  (setf (aref chars j) (code-char (u16 d (+ abs-off (* j 2))))))
                                (coerce chars 'string))
                              ;; MacRoman / 1-byte
                              (let ((chars (make-array len :element-type 'character)))
                                (dotimes (j len)
                                  (setf (aref chars j) (code-char (aref d (+ abs-off j)))))
                                (coerce chars 'string))))))))
      (dolist (name-id '(1 2 4 6))
        (let ((val (or (decode 3 1 #x409 name-id)
                       (decode 1 0 0 name-id))))
          (when val
            (push (cons (format nil "~d" name-id) val) result))))
      (nreverse result))))