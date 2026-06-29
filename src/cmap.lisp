;;;; cmap.lisp — cmap header (strong-tier kernel for W2) + format 6 reference.
;;;;
;;;; (cmap-subtables font) lists the subtables; each FORMAT body parser is an
;;;; independent swarm unit (src/tables/cmap-<fmt>.lisp) taking (d off) and
;;;; returning a hash-table codepoint->gid, EXCLUDING gid 0 (fontTools policy).
;;;; Format 6 is the proven reference; formats 4 and 12 are swarmed.
(in-package #:scribe)

(defun cmap-subtables (font)
  "List of (platform-id encoding-id format absolute-offset) for each subtable."
  (let* ((d (font-data font)) (co (req-table font "cmap"))
         (n (u16 d (+ co 2))) (out '()))
    (dotimes (i n (nreverse out))
      (let* ((rec (+ co 4 (* i 8)))
             (pid (u16 d rec)) (eid (u16 d (+ rec 2)))
             (suboff (+ co (u32 d (+ rec 4)))))
        (push (list pid eid (u16 d suboff) suboff) out)))))

(defun parse-cmap-6 (d off)            ; REFERENCE unit (trimmed array)
  "Format 6: firstCode@6, entryCount@8, then entryCount u16 glyph ids @10."
  (let ((ht (make-hash-table :test 'eql))
        (first (u16 d (+ off 6))) (count (u16 d (+ off 8))))
    (dotimes (i count ht)
      (let ((gid (u16 d (+ off 10 (* i 2)))))
        (when (plusp gid) (setf (gethash (+ first i) ht) gid))))))
