;;;; src/tables/os_2.lisp — SWARM W1 unit. Implement (parse-os_2 font) -> alist
;;;; (field-string . value) matching the "os_2." rows in the vendored oracle
;;;; inspect/vectors/tables/<font>.tsv. See $unit.task.md for the byte layout.
;;;; Pure CL only. Readers in scribe: u8/u16/s16/u32 (big-endian, absolute off);
;;;; (req-table font "os_2") -> table offset. STUB returns nil (gate: all-failed).
(in-package #:scribe)

(defun parse-os_2 (font)
  (let* ((d (font-data font))
         (off (req-table font "OS/2")))
    `(("usWeightClass" . ,(u16 d (+ off 4)))
      ("usWidthClass" . ,(u16 d (+ off 6)))
      ("sTypoAscender" . ,(s16 d (+ off 68)))
      ("sTypoDescender" . ,(s16 d (+ off 70)))
      ("sTypoLineGap" . ,(s16 d (+ off 72)))
      ("usWinAscent" . ,(u16 d (+ off 74)))
      ("usWinDescent" . ,(u16 d (+ off 76)))
      ("sxHeight" . ,(s16 d (+ off 86)))
      ("sCapHeight" . ,(s16 d (+ off 88)))
      ("xAvgCharWidth" . ,(s16 d (+ off 2)))
      ("fsSelection" . ,(u16 d (+ off 62))))))