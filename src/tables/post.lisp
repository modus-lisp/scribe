;;;; src/tables/post.lisp — SWARM W1 unit. Implement (parse-post font) -> alist
;;;; (field-string . value) matching the "post." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv. See $unit.task.md for the byte layout.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "post") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-post (font)
  (declare (ignore font))
  nil)
