;;;; font.lisp — sfnt / OpenType parsing.
;;;;
;;;; STRONG-TIER KERNEL (this file's reader + directory + head/maxp/hhea) is the
;;;; carve that makes the table-decoder swarm fillable; see SWARM.md. Each table
;;;; parser is an independent unit: (parse-<table> font) -> alist (field . value)
;;;; whose field names match inspect/vectors/tables/<font>.tsv. head/maxp/hhea
;;;; are implemented as the reference; hmtx/OS_2/post/name are swarm stubs.
(in-package #:scribe)

;;; ---- big-endian readers over the raw font bytes (absolute offsets) ----
(declaim (inline u8 u16 s16 u32 i16))
(defun u8  (d o) (aref d o))
(defun u16 (d o) (logior (ash (aref d o) 8) (aref d (1+ o))))
(defun s16 (d o) (let ((v (u16 d o))) (if (>= v #x8000) (- v #x10000) v)))
(defun u32 (d o) (logior (ash (aref d o) 24) (ash (aref d (+ o 1)) 16)
                         (ash (aref d (+ o 2)) 8) (aref d (+ o 3))))
(defun tag (d o) (map 'string #'code-char (list (aref d o) (aref d (+ o 1))
                                                (aref d (+ o 2)) (aref d (+ o 3)))))

;;; ---- the font + table directory ----
(defstruct font
  data            ; (simple-array (unsigned-byte 8))
  tables          ; hash: tag-string -> (offset . length)
  units-per-em num-glyphs num-h-metrics index-to-loc-format
  %cff            ; cached parsed CFF (lazy) for OTTO fonts
  %fvar %avar     ; cached variation axes + avar segment maps (lazy)
  %gdef)          ; cached parsed GDEF (lazy); :none if absent

(defun font-table (font tag-str)
  "Return (values offset length) for TAG-STR, or NIL if absent."
  (let ((e (gethash tag-str (font-tables font))))
    (when e (values (car e) (cdr e)))))

(defun req-table (font tag-str)
  (or (font-table font tag-str)
      (error "scribe: required table ~s missing" tag-str)))

(defun open-font (bytes)
  "Parse an sfnt/OpenType font. Reads the table directory and the three
   foundation tables (head/maxp/hhea) that everything else needs."
  (let* ((d0 (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         ;; Transparently accept .woff2 / .woff: detect the signature and
         ;; decompress to a standard in-memory sfnt before parsing.
         (d (maybe-decompress-web-font d0))
         (ver (u32 d 0)))
    (when (= ver #x74746366) (error "scribe: TrueType Collections not yet supported"))
    (unless (or (= ver #x00010000)        ; TrueType outlines
                (= ver #x4F54544F)        ; 'OTTO' = CFF outlines
                (= ver #x74727565))       ; 'true'
      (error "scribe: not an sfnt (version #x~x)" ver))
    (let* ((n (u16 d 4))
           (tbls (make-hash-table :test 'equal)))
      (dotimes (i n)
        (let ((rec (+ 12 (* i 16))))
          (setf (gethash (tag d rec) tbls)
                (cons (u32 d (+ rec 8)) (u32 d (+ rec 12))))))
      (let ((font (make-font :data d :tables tbls)))
        ;; eagerly cache the fields glyf/loca/hmtx all depend on
        (let ((h (cdr (assoc "unitsPerEm" (parse-head font) :test #'string=))))
          (setf (font-units-per-em font) h))
        (setf (font-index-to-loc-format font)
              (cdr (assoc "indexToLocFormat" (parse-head font) :test #'string=)))
        (setf (font-num-glyphs font)
              (cdr (assoc "numGlyphs" (parse-maxp font) :test #'string=)))
        (setf (font-num-h-metrics font)
              (cdr (assoc "numberOfHMetrics" (parse-hhea font) :test #'string=)))
        font))))

;;; ===========================================================================
;;; Table decoders — each (parse-X font) -> alist (field-string . value).
;;; ===========================================================================

(defun parse-head (font)               ; REFERENCE unit
  (let ((d (font-data font)) (o (req-table font "head")))
    (list (cons "unitsPerEm"       (u16 d (+ o 18)))
          (cons "indexToLocFormat" (s16 d (+ o 50)))
          (cons "xMin"             (s16 d (+ o 36)))
          (cons "yMin"             (s16 d (+ o 38)))
          (cons "xMax"             (s16 d (+ o 40)))
          (cons "yMax"             (s16 d (+ o 42)))
          (cons "macStyle"         (u16 d (+ o 44)))
          (cons "lowestRecPPEM"    (u16 d (+ o 46)))
          (cons "flags"            (u16 d (+ o 16)))
          (cons "magicNumber"      (u32 d (+ o 12))))))

(defun parse-maxp (font)               ; REFERENCE unit
  (let ((d (font-data font)) (o (req-table font "maxp")))
    (list (cons "numGlyphs" (u16 d (+ o 4))))))

(defun parse-hhea (font)               ; REFERENCE unit
  (let ((d (font-data font)) (o (req-table font "hhea")))
    (list (cons "ascent"              (s16 d (+ o 4)))
          (cons "descent"             (s16 d (+ o 6)))
          (cons "lineGap"             (s16 d (+ o 8)))
          (cons "advanceWidthMax"     (u16 d (+ o 10)))
          (cons "minLeftSideBearing"  (s16 d (+ o 12)))
          (cons "minRightSideBearing" (s16 d (+ o 14)))
          (cons "xMaxExtent"          (s16 d (+ o 16)))
          (cons "numberOfHMetrics"    (u16 d (+ o 34))))))

;;; ---- SWARM W1 table decoders live one-per-file in src/tables/ so collection
;;; ---- is a single-file copy: src/tables/{hmtx,os_2,post,name}.lisp ----

;;; ---- cmap (W2) + outlines (strong-tier) stay as later milestones ----
(defun font-glyph-index (font codepoint)
  (declare (ignore font codepoint))
  (error "scribe: cmap not yet implemented (W2)"))
(defun glyph-outline (font gid &key variation)
  (declare (ignore font gid variation))
  (error "scribe: glyf/loca outlines not yet implemented (strong-tier)"))
