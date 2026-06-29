;;;; src/tables/cmap-4.lisp — SWARM W2 unit. Implement (parse-cmap-4 d off) -> a
;;;; hash-table (eql) codepoint->gid for one cmap subtable at absolute byte
;;;; offset OFF in font bytes D. EXCLUDE gid 0 (fontTools policy). See task file.
;;;; Readers: (u8 d o)(u16 d o)(s16 d o)(u32 d o) big-endian absolute. Pure CL.
(in-package #:scribe)

(defun parse-cmap-4 (d off)
  (declare (ignore d off))
  (make-hash-table :test 'eql))   ; STUB -> gate reports all-failed (non-vacuous)
