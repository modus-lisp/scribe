;;;; src/tables/cmap-0.lisp — (parse-cmap-0 d off) -> a
;;;; hash-table (eql) codepoint->gid for one cmap subtable at absolute byte
;;;; offset OFF in font bytes D. EXCLUDE gid 0 (fontTools policy).
;;;; Readers: (u8 d o)(u16 d o)(s16 d o)(u32 d o) big-endian absolute. Pure CL.
;;;;
;;;; Format 0 (byte encoding table): a fixed 256-entry byte array mapping each
;;;; single-byte code directly to a glyph id.  Ubiquitous on Type1/CFF and
;;;; TrueType SUBSETS that keep only a (1,0) Mac-Roman-style table; without it a
;;;; PDF's embedded subset cmap is unreadable and every glyph resolves to .notdef.
(in-package #:scribe)

(defun parse-cmap-0 (d off)
  "Format 0: format@0, length@2, language@4, then 256 u8 glyph ids @6."
  (let ((ht (make-hash-table :test 'eql)))
    (dotimes (c 256 ht)
      (let ((gid (u8 d (+ off 6 c))))
        (when (plusp gid) (setf (gethash c ht) gid))))))
