;;;; src/tables/cmap-12.lisp — SWARM W2 unit. Implement (parse-cmap-12 d off) -> a
;;;; hash-table (eql) codepoint->gid for one cmap subtable at absolute byte
;;;; offset OFF in font bytes D. EXCLUDE gid 0 (fontTools policy). See task file.
;;;; Readers: (u8 d o)(u16 d o)(s16 d o)(u32 d o) big-endian absolute. Pure CL.
(in-package #:scribe)

(defun parse-cmap-12 (d off)
  (let* ((num-groups (u32 d (+ off 12)))
         (ht (make-hash-table :test 'eql)))
    (dotimes (g num-groups ht)
      (let* ((rec-base (+ off 16 (* g 12)))
             (start-code (u32 d rec-base))
             (end-code   (u32 d (+ rec-base 4)))
             (start-gid  (u32 d (+ rec-base 8))))
        (loop for c from start-code to end-code
              for gid = (+ start-gid (- c start-code))
              when (plusp gid) do (setf (gethash c ht) gid))))))
