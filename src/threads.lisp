;;;; threads.lisp — drawing text on more than one thread.
;;;;
;;;; scribe keeps state in globals: scratch for the rasterizer (the pen's last point), gamma
;;;; tables memoized per gamma and colour, and caches -- hinter programs and hinted outlines,
;;;; CPAL/COLR tables, OpenType coverage and class tables, matched and fallback fonts.  On a
;;;; Lisp whose threads share memory that is fine.  On modus they share NOTHING: a thread's own
;;;; objects may not be stored into a global, so drawing text on any thread but main is refused
;;;; at the first cache fill.  WITH-THREAD-STATE gives the body a private copy of all of it --
;;;; every one of those specials bound afresh, which on modus is storage of the thread's own --
;;;; so a thread that draws text all the time (a compositor) can.  Fonts are not state: they
;;;; are read-only once opened, and are shared freely.

(in-package #:scribe)

(defun %copy-table (h)
  (let ((c (make-hash-table :test (hash-table-test h) :size (max 16 (hash-table-count h)))))
    (maphash (lambda (k v) (setf (gethash k c) v)) h)
    c))

(defmacro with-thread-state (() &body body)
  "BODY with scribe's caches and scratch private to this thread.  Costs one set of empty caches
   (they refill as the thread draws); a thread that draws once does not need it."
  `(let ((*lpx* 0d0) (*lpy* 0d0)
         (*g-gamma* nil) (*g-fg-key* -1)
         (*g-bg* (make-array 256 :element-type 'double-float))
         (*g-fg* (make-array 3 :element-type 'double-float))
         (*g-out* (make-array 4097 :element-type '(unsigned-byte 8)))
         (*cpal-cache* (make-hash-table :test 'eq))
         (*colr-cache* (make-hash-table :test 'eq))
         (*hinter-cache* (make-hash-table :test 'equal))
         (*hinted-cache* (make-hash-table :test 'equal))
         ;; fonts are shared once opened, so the caller's opened faces are COPIED: a page
         ;; worker does not read megabytes of CJK fallback faces main already has.  Their
         ;; shaping caches are this thread's own (*OTL-CACHES*).
         (*font-cache* (%copy-table *font-cache*))
         (*fallback-cache* (%copy-table *fallback-cache*))
         (*otl-caches* (make-hash-table :test 'eq))
         (*cov-cache* (make-hash-table :test 'eq))
         (*classdef-cache* (make-hash-table :test 'eq)))
     ,@body))

(export 'with-thread-state)
