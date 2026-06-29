;;;; src/tables/os_2.lisp — SWARM W1 unit. Implement (parse-os_2 font) -> alist
;;;; (field-string . value) matching the "os_2." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv. See $unit.task.md for the byte layout.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "os_2") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-os_2 (font)
  (declare (ignore font))
  nil)
