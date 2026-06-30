;;;; src/tables/hmtx.lisp — (parse-hmtx font) -> alist
;;;; (field-string . value) matching the "hmtx." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "hmtx") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-hmtx (font)
  (let* ((off (req-table font "hmtx"))
         (data (font-data font))
         (nh (cdr (assoc "numberOfHMetrics" (parse-hhea font) :test #'string=)))
         (ng (cdr (assoc "numGlyphs" (parse-maxp font) :test #'string=)))
         (limit (min ng 300))
         (result nil))
    (dotimes (gid limit (nreverse result))
      (if (< gid nh)
          (let ((adv (u16 data (+ off (* gid 4))))
                (lsb (s16 data (+ off (* gid 4) 2))))
            (push (cons (format nil "advance.~A" gid) adv) result)
            (push (cons (format nil "lsb.~A" gid) lsb) result))
          (let* ((last-adv-off (+ off (* (1- nh) 4)))
                 (adv (u16 data last-adv-off))
                 (lsb-off (+ off (* nh 4) (* (- gid nh) 2)))
                 (lsb (s16 data lsb-off)))
            (push (cons (format nil "advance.~A" gid) adv) result)
            (push (cons (format nil "lsb.~A" gid) lsb) result))))))
