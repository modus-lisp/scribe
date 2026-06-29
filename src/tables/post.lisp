;;;; src/tables/post.lisp — SWARM W1 unit. Implement (parse-post font) -> alist
;;;; (field-string . value) matching the "post." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv. See $unit.task.md for the byte layout.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "post") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-post (font)
  (let ((d (font-data font))
        (o (req-table font "post")))
    (flet ((s32 (off)
             (let ((v (u32 d off)))
               (if (>= v #x80000000) (- v #x100000000) v))))
      (let ((raw-angle (s32 (+ o 4))))
        (list (cons "italicAngle" (if (zerop (mod raw-angle 65536))
                                      (truncate (/ raw-angle 65536))
                                      (/ raw-angle 65536.0)))
              (cons "underlinePosition" (s16 d (+ o 8)))
              (cons "underlineThickness" (s16 d (+ o 10)))
              (cons "isFixedPitch" (u32 d (+ o 12))))))))
