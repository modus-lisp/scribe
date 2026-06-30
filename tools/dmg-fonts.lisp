;;;; dmg-fonts.lisp — carve font files out of an Apple .dmg font installer.
;;;;
;;;; Pure Common Lisp, clean-room. No FFI, no external compression libraries.
;;;; The only dependency loaded is `scribe` itself (the consumer), used purely to
;;;; *validate* carved fonts. Everything in the extraction pipeline — DEFLATE
;;;; inflate, base64, UDIF, HFS+ scan, xar, gzip, cpio, sfnt carve — is our own.
;;;;
;;;; Pipeline:  DMG (UDIF) -> reconstruct HFS+ image -> find xar! .pkg
;;;;            -> zlib TOC -> Payload (gzip) -> gunzip -> cpio -> carve sfnt
;;;;
;;;; Usage:  sbcl --script dmg-fonts.lisp <DMG-path> <output-dir>
;;;;
;;;; All multi-byte integers in the on-disk structures are BIG-ENDIAN unless a
;;;; reader is explicitly named *-le.

(load (merge-pathnames "../src/deflate.lisp" *load-pathname*))

(defpackage #:dmg-fonts
  (:use #:cl)
  (:export #:extract-fonts #:main))
(in-package #:dmg-fonts)

(declaim (optimize (speed 2) (safety 1)))

(deftype ub8v () '(simple-array (unsigned-byte 8) (*)))

;;; ---------------------------------------------------------------------------
;;; I/O + big-endian readers
;;; ---------------------------------------------------------------------------

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun write-file-bytes (path bytes &key (start 0) (end (length bytes)))
  (with-open-file (s path :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence bytes s :start start :end end)))

(declaim (inline be-u16 be-u32 be-u64 le-u16))
(defun be-u16 (d o) (logior (ash (aref d o) 8) (aref d (+ o 1))))
(defun be-u32 (d o)
  (logior (ash (aref d o) 24) (ash (aref d (+ o 1)) 16)
          (ash (aref d (+ o 2)) 8) (aref d (+ o 3))))
(defun be-u64 (d o)
  (logior (ash (be-u32 d o) 32) (be-u32 d (+ o 4))))
(defun le-u16 (d o) (logior (aref d o) (ash (aref d (+ o 1)) 8)))

(defun ascii-at-p (d o str)
  "Does ASCII STR appear at offset O in D?"
  (let ((n (length str)))
    (and (<= (+ o n) (length d))
         (loop for i below n
               always (= (aref d (+ o i)) (char-code (char str i)))))))

(defun find-ascii (d str &optional (start 0))
  "Index of first occurrence of ASCII STR in D at or after START, else NIL."
  (let* ((n (length str)) (b0 (char-code (char str 0)))
         (limit (- (length d) n)))
    (loop for i from start to limit
          when (and (= (aref d i) b0) (ascii-at-p d i str))
            do (return i))))

;;; ---------------------------------------------------------------------------
;;; base64 (standard alphabet, whitespace-tolerant, '=' padding)
;;; ---------------------------------------------------------------------------

(defparameter *b64-dec*
  (let ((tab (make-array 256 :element-type 'fixnum :initial-element -1))
        (alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (loop for ch across alpha for i from 0
          do (setf (aref tab (char-code ch)) i))
    tab))

(defun base64-decode (src &key (start 0) (end (length src)))
  "Decode base64 from SRC (a string or ub8 vector) over [START,END).
   Skips whitespace; '=' terminates."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (acc 0) (nbits 0))
    (flet ((byte-at (i)
             (let ((c (aref src i)))
               (if (characterp c) (char-code c) c))))
      (loop for i from start below end
            for c = (byte-at i)
            do (cond
                 ((= c (char-code #\=)) (return))
                 ((<= c 32))            ; whitespace / control: skip
                 (t (let ((v (aref *b64-dec* c)))
                      (when (>= v 0)
                        (setf acc (logior (ash acc 6) v))
                        (incf nbits 6)
                        (when (>= nbits 8)
                          (decf nbits 8)
                          (vector-push-extend (logand (ash acc (- nbits)) #xff) out))))))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; ---------------------------------------------------------------------------
;;; UDIF: koly trailer -> blkx (mish) tables -> reconstruct HFS+ image
;;; ---------------------------------------------------------------------------

(defconstant +sector+ 512)

(defun extract-mish-blocks (dmg)
  "Scan the XML plist for <data> blocks whose decoded bytes start with 'mish'."
  (let* ((n (length dmg))
         (trailer-off (- n 512)))
    (unless (ascii-at-p dmg trailer-off "koly")
      (error "dmg: no koly trailer at end of file"))
    (let* ((xml-off (be-u64 dmg (+ trailer-off 216)))
           (xml-len (be-u64 dmg (+ trailer-off 224)))
           (xml-end (+ xml-off xml-len))
           (open "<data>") (close "</data>")
           (mishes '()))
      (let ((p xml-off))
        (loop
          (let ((ds (find-ascii dmg open p)))
            (unless (and ds (< ds xml-end)) (return))
            (let* ((dstart (+ ds (length open)))
                   (de (find-ascii dmg close dstart)))
              (unless (and de (< de xml-end)) (return))
              (let ((blob (base64-decode dmg :start dstart :end de)))
                (when (and (>= (length blob) 4) (ascii-at-p blob 0 "mish"))
                  (push blob mishes)))
              (setf p (+ de (length close)))))))
      (nreverse mishes))))

(defun reconstruct-image (dmg)
  "Apply every mish chunk to rebuild the HFS+ disk image. Returns the image
   as a ub8 vector sized to the highest output offset reached."
  (let* ((trailer-off (- (length dmg) 512))
         (data-fork-off (be-u64 dmg (+ trailer-off 24)))
         (mishes (extract-mish-blocks dmg)))
    ;; First pass: determine the total output size (max end offset).
    (let ((max-end 0))
      (dolist (m mishes)
        (let* ((base-sector (be-u64 m 8))
               (nchunks (be-u32 m 200)))
          (dotimes (i nchunks)
            (let* ((eo (+ 204 (* i 40)))
                   (etype (be-u32 m eo))
                   (sector-num (be-u64 m (+ eo 8)))
                   (sector-cnt (be-u64 m (+ eo 16))))
              (when (= etype #xffffffff) (return))
              (let ((out-off (* (+ base-sector sector-num) +sector+))
                    (out-len (* sector-cnt +sector+)))
                (setf max-end (max max-end (+ out-off out-len))))))))
      (let ((image (make-array max-end :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
        (dolist (m mishes)
          (let* ((base-sector (be-u64 m 8))
                 (nchunks (be-u32 m 200)))
            (dotimes (i nchunks)
              (let* ((eo (+ 204 (* i 40)))
                     (etype (be-u32 m eo))
                     (sector-num (be-u64 m (+ eo 8)))
                     (comp-off (be-u64 m (+ eo 24)))
                     (comp-len (be-u64 m (+ eo 32)))
                     (out-off (* (+ base-sector sector-num) +sector+))
                     (src-start (+ data-fork-off comp-off))
                     (src-end (+ src-start comp-len)))
                (cond
                  ((= etype #xffffffff) (return))     ; terminator
                  ((or (= etype #x00000000) (= etype #x00000002)) nil) ; zero/ignore
                  ((= etype #x00000001)               ; raw copy
                   (replace image dmg :start1 out-off :start2 src-start :end2 src-end))
                  ((= etype #x80000005)               ; zlib
                   (let ((plain (deflate:zlib-decompress dmg :start src-start :end src-end)))
                     (replace image plain :start1 out-off)))
                  ((= etype #x80000006)               ; bzip2 (not expected here)
                   (error "dmg: bzip2 chunk encountered (unsupported)"))
                  ((= etype #x80000007)               ; LZFSE (not expected here)
                   (error "dmg: LZFSE chunk encountered (unsupported)"))
                  (t                                  ; unknown: skip but note
                   (warn "dmg: unknown chunk type #x~x at mish chunk ~d" etype i)))))))
        image))))

;;; ---------------------------------------------------------------------------
;;; xar: locate 'xar!', zlib-inflate the TOC, find Payload, slice it out
;;; ---------------------------------------------------------------------------

(defun xar-find-payload (image)
  "Find the xar archive in IMAGE, parse its TOC, and return the raw Payload
   bytes (a fresh ub8 vector)."
  (let ((xoff (find-ascii image "xar!")))
    (unless xoff (error "xar: 'xar!' signature not found in image"))
    (let* ((header-size (be-u16 image (+ xoff 4)))
           (toc-comp-len (be-u64 image (+ xoff 8)))
           (toc-start (+ xoff header-size))
           (toc-end (+ toc-start toc-comp-len))
           (heap (+ toc-start toc-comp-len))
           (toc-bytes (deflate:zlib-decompress image :start toc-start :end toc-end))
           (toc (map 'string #'code-char toc-bytes)))
      (multiple-value-bind (offset length) (toc-payload-extent toc)
        (unless offset (error "xar: no Payload entry found in TOC"))
        (let ((start (+ heap offset)))
          (subseq image start (+ start length)))))))

(defun xml-tag-int (xml tag &optional (start 0))
  "Return the integer inside <TAG>...</TAG> at/after START, and the end index."
  (let* ((open (format nil "<~a>" tag))
         (close (format nil "</~a>" tag))
         (os (search open xml :start2 start)))
    (when os
      (let* ((vs (+ os (length open)))
             (ve (search close xml :start2 vs)))
        (when ve
          (values (parse-integer xml :start vs :end ve :junk-allowed t)
                  (+ ve (length close))))))))

(defun rsearch (needle haystack &key (end (length haystack)))
  "Index of the last occurrence of NEEDLE in HAYSTACK before END, else NIL."
  (let ((best nil) (p 0))
    (loop
      (let ((i (search needle haystack :start2 p :end2 end)))
        (unless i (return best))
        (setf best i p (1+ i))))))

(defun toc-payload-extent (toc)
  "Find the <file> named 'Payload' in the TOC XML and return (values offset
   length) of its <data> child. In xar TOCs the <data> block is the FIRST child
   of the <file> element — it appears BEFORE <name>...</name>. So: locate the
   name, walk back to the enclosing <file opening, then take the <data> that
   follows that <file (which is this file's own data, before any child <file)."
  (let ((np (search "<name>Payload</name>" toc)))
    (unless np (return-from toc-payload-extent (values nil nil)))
    (let ((file-open (rsearch "<file " toc :end np)))
      (unless file-open (return-from toc-payload-extent (values nil nil)))
      (let ((datap (search "<data>" toc :start2 file-open :end2 np)))
        (unless datap (return-from toc-payload-extent (values nil nil)))
        (let ((offset (xml-tag-int toc "offset" datap))
              (length (xml-tag-int toc "length" datap)))
          (values offset length))))))

;;; ---------------------------------------------------------------------------
;;; cpio: parse odc (070707) / newc (070701) to recover filenames (nice-to-have)
;;; ---------------------------------------------------------------------------

(defun parse-cpio (data)
  "Parse a cpio archive. Returns a list of (name . (start . length)) for each
   regular file, where start/length index into DATA. Supports odc and newc.
   Returns NIL if the archive isn't recognized."
  (cond
    ((ascii-at-p data 0 "070707") (parse-cpio-odc data))
    ((ascii-at-p data 0 "070701") (parse-cpio-newc data))
    ((ascii-at-p data 0 "070702") (parse-cpio-newc data))
    (t nil)))

(defun oct-field (data o n)
  (parse-integer (map 'string #'code-char (subseq data o (+ o n))) :radix 8))
(defun hex-field (data o n)
  (parse-integer (map 'string #'code-char (subseq data o (+ o n))) :radix 16))

(defun parse-cpio-odc (data)
  "Portable ASCII (odc) cpio: 6-byte magic + 13 octal fields (each 6 bytes
   except dev/ino... actually odc layout: magic6 dev6 ino6 mode6 uid6 gid6
   nlink6 rdev6 mtime11 namesize6 filesize11). Total header 76 bytes."
  (let ((p 0) (files '()))
    (loop
      (unless (and (<= (+ p 76) (length data)) (ascii-at-p data p "070707"))
        (return))
      (let* ((namesize (oct-field data (+ p 59) 6))
             (filesize (oct-field data (+ p 65) 11))
             (name-start (+ p 76))
             (name (map 'string #'code-char
                        (subseq data name-start (+ name-start (1- namesize)))))
             (data-start (+ name-start namesize)))
        (when (string= name "TRAILER!!!") (return))
        (when (plusp filesize)
          (push (cons name (cons data-start filesize)) files))
        (setf p (+ data-start filesize))))
    (nreverse files)))

(defun align4 (n) (* 4 (ceiling n 4)))

(defun parse-cpio-newc (data)
  "newc / crc (070701 / 070702): 110-byte header, all fields 8 hex. name and
   data each padded to a 4-byte boundary (padding measured from start of file)."
  (let ((p 0) (files '()))
    (loop
      (unless (and (<= (+ p 110) (length data))
                   (or (ascii-at-p data p "070701") (ascii-at-p data p "070702")))
        (return))
      (let* ((filesize (hex-field data (+ p 54) 8))
             (namesize (hex-field data (+ p 94) 8))
             (name-start (+ p 110))
             (name (map 'string #'code-char
                        (subseq data name-start (+ name-start (1- namesize)))))
             (data-start (align4 (+ name-start namesize)))
             (next (align4 (+ data-start filesize))))
        (when (string= name "TRAILER!!!") (return))
        (when (plusp filesize)
          (push (cons name (cons data-start filesize)) files))
        (setf p next)))
    (nreverse files)))

;;; ---------------------------------------------------------------------------
;;; sfnt carving
;;; ---------------------------------------------------------------------------

(defparameter *sfnt-sigs*
  ;; (byte-pattern . description)
  (list (cons #(#x00 #x01 #x00 #x00) :truetype)
        (cons (map 'vector #'char-code "OTTO") :cff)
        (cons (map 'vector #'char-code "true") :truett)))

(defun sfnt-len-at (data p)
  "If a valid sfnt header begins at P, return the font length (4-aligned),
   else NIL. Validates numTables and table directory ranges."
  (let ((n (length data)))
    (when (< (+ p 12) n)
      (let ((num-tables (be-u16 data (+ p 4))))
        (when (and (>= num-tables 1) (<= num-tables 60)
                   (<= (+ p 12 (* num-tables 16)) n))
          (let ((maxend 0) (ok t))
            (dotimes (i num-tables)
              (let* ((rec (+ p 12 (* i 16)))
                     (toff (be-u32 data (+ rec 8)))
                     (tlen (be-u32 data (+ rec 12)))
                     (abs-end (+ p toff tlen)))
                ;; offsets are relative to font start (p); sanity-check range
                (when (or (> (+ p toff) n) (> abs-end n) (> toff (- n p)))
                  (setf ok nil) (return))
                (setf maxend (max maxend (+ toff tlen)))))
            (when (and ok (plusp maxend))
              (let ((flen (align4 maxend)))
                (when (<= (+ p flen) n) flen)))))))))

(defun match-sig (data p)
  (loop for (pat . kind) in *sfnt-sigs*
        when (and (<= (+ p (length pat)) (length data))
                  (loop for j below (length pat)
                        always (= (aref data (+ p j)) (aref pat j))))
          do (return kind)))

(defun carve-fonts (data)
  "Scan DATA for sfnt fonts. Returns a list of (start . length)."
  (let ((found '()) (p 0) (n (length data)))
    (loop while (< p (- n 12)) do
      (if (match-sig data p)
          (let ((flen (sfnt-len-at data p)))
            (if flen
                (progn (push (cons p flen) found)
                       (incf p flen))    ; skip past this font
                (incf p)))
          (incf p)))
    (nreverse found)))

;;; ---------------------------------------------------------------------------
;;; scribe validation + name extraction
;;; ---------------------------------------------------------------------------

(defvar *scribe-loaded* nil)
(defun ensure-scribe ()
  (unless *scribe-loaded*
    (require :asdf)
    (let ((reg (find-symbol "*CENTRAL-REGISTRY*" :asdf))
          (loadsys (find-symbol "LOAD-SYSTEM" :asdf)))
      (pushnew #p"/home/claude/scribe/" (symbol-value reg) :test #'equal)
      (handler-bind ((warning #'muffle-warning))
        (funcall loadsys "scribe")))
    (setf *scribe-loaded* t)))

(defun validate-font (blob)
  "Return (values font upem) if scribe parses BLOB, else NIL."
  (handler-case
      (let* ((font (funcall (find-symbol "OPEN-FONT" :scribe) blob))
             (upem (funcall (find-symbol "FONT-UNITS-PER-EM" :scribe) font)))
        (if (and (integerp upem) (<= 16 upem 16384))
            (values font upem)
            nil))
    (error () nil)
    (warning () nil)))

(defun font-full-name (blob)
  "Extract name table id 4 (full font name), trying Windows/Unicode and Mac
   encodings. Returns a string or NIL. Self-contained (doesn't need scribe)."
  (handler-case
      (let* ((d blob)
             (num-tables (be-u16 d 4))
             (name-off nil))
        (dotimes (i num-tables)
          (let ((rec (+ 12 (* i 16))))
            (when (ascii-at-p d rec "name")
              (setf name-off (be-u32 d (+ rec 8))))))
        (when name-off
          (let* ((count (be-u16 d (+ name-off 2)))
                 (string-off (+ name-off (be-u16 d (+ name-off 4))))
                 (best nil))
            (dotimes (i count)
              (let* ((rec (+ name-off 6 (* i 12)))
                     (platform (be-u16 d rec))
                     (name-id (be-u16 d (+ rec 6)))
                     (len (be-u16 d (+ rec 8)))
                     (off (be-u16 d (+ rec 10))))
                (when (= name-id 4)
                  (let ((s (+ string-off off)))
                    (cond
                      ((= platform 3)   ; Windows: UTF-16BE
                       (setf best
                             (with-output-to-string (o)
                               (loop for k from 0 below len by 2
                                     do (write-char (code-char (be-u16 d (+ s k))) o)))))
                      ((and (= platform 1) (null best)) ; Mac Roman (ASCII-ish)
                       (setf best
                             (map 'string #'code-char (subseq d s (+ s len))))))))))
            best)))
    (error () nil)))

(defun sanitize-filename (name)
  (let ((s (map 'string (lambda (c) (if (or (alphanumericp c)
                                            (member c '(#\- #\_ #\. #\Space)))
                                        c #\_))
                name)))
    (string-trim " " s)))

;;; ---------------------------------------------------------------------------
;;; Driver
;;; ---------------------------------------------------------------------------

(defun extract-fonts (dmg-path out-dir &key (validate t))
  "Full pipeline. Returns a list of plists describing each carved font."
  (ensure-directories-exist (ensure-dir-pathname out-dir))
  (format t "~&[1/6] reading DMG ~a~%" dmg-path)
  (let ((dmg (read-file-bytes dmg-path)))
    (format t "      ~d bytes~%" (length dmg))
    (format t "[2/6] reconstructing HFS+ image from UDIF blkx tables~%")
    (let ((image (reconstruct-image dmg)))
      (format t "      image ~d bytes~%" (length image))
      (format t "[3/6] locating xar archive + reading Payload~%")
      (let ((payload (xar-find-payload image)))
        (format t "      Payload ~d bytes (magic ~2,'0x ~2,'0x)~%"
                (length payload) (aref payload 0) (aref payload 1))
        (format t "[4/6] gunzip Payload -> cpio~%")
        (let ((cpio (if (and (= (aref payload 0) #x1f) (= (aref payload 1) #x8b))
                        (deflate:gzip-decompress payload)
                        payload)))
          (format t "      cpio ~d bytes (magic ~a)~%"
                  (length cpio)
                  (map 'string #'code-char (subseq cpio 0 (min 6 (length cpio)))))
          (format t "[5/6] parsing cpio (filenames, nice-to-have)~%")
          (let ((cpio-files (or (parse-cpio cpio) '())))
            (format t "      ~d cpio file entries~%" (length cpio-files))
            (format t "[6/6] carving sfnt fonts~%")
            (let ((carved (carve-fonts cpio))
                  (results '())
                  (counter 0))
              (format t "      ~d sfnt candidates carved~%" (length carved))
              (when validate (ensure-scribe))
              (dolist (cl carved)
                (let* ((start (car cl)) (len (cdr cl)))
                  (let ((blob (subseq cpio start (+ start len))))
                    (multiple-value-bind (font upem)
                        (if validate (validate-font blob) (values t nil))
                      (when (or font (not validate))
                        (incf counter)
                        (let* ((full (font-full-name blob))
                               (cpio-name (cpio-name-covering cpio-files start))
                               (base (cond
                                       (cpio-name (file-namestring cpio-name))
                                       (full (concatenate 'string
                                               (sanitize-filename full) ".font"))
                                       (t (format nil "font_~3,'0d" counter))))
                               (path (font-output-path out-dir base full)))
                          (write-file-bytes path blob)
                          (push (list :path path :start start :length len
                                      :upem upem :name full :cpio cpio-name)
                                results)))))))
              (setf results (nreverse results))
              (format t "~&==> wrote ~d valid fonts to ~a~%" (length results) out-dir)
              results)))))))

(defun cpio-name-covering (cpio-files offset)
  "If a cpio file's data range covers OFFSET, return its name."
  (loop for (name . (start . len)) in cpio-files
        when (and (<= start offset) (< offset (+ start len)))
          do (return name)))

(defun ensure-dir-pathname (dir)
  "Coerce DIR (string or pathname) to a directory pathname with trailing slash."
  (let ((s (if (pathnamep dir) (namestring dir) dir)))
    (pathname (if (and (plusp (length s)) (char/= (char s (1- (length s))) #\/))
                  (concatenate 'string s "/")
                  s))))

(defun basename-of (name)
  "Return the final path component of NAME (strip any directory parts)."
  (let ((slash (position #\/ name :from-end t)))
    (if slash (subseq name (1+ slash)) name)))

(defun font-output-path (out-dir base full)
  (declare (ignore full))
  (let* ((b (basename-of base))
         (ext (if (find #\. b) "" ".ttf"))
         (fname (concatenate 'string b ext)))
    (merge-pathnames fname (ensure-dir-pathname out-dir))))

(defun main ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when (< (length args) 2)
      (format *error-output* "usage: dmg-fonts.lisp <DMG-path> <output-dir>~%")
      (sb-ext:exit :code 2))
    (let ((dmg (first args)) (out (second args)))
      (let ((start (get-internal-real-time)))
        (let ((results (extract-fonts dmg out)))
          (format t "~&--- carved fonts ---~%")
          (dolist (r results)
            (format t "  ~a  (upem=~a, len=~a)~%"
                    (getf r :path) (getf r :upem) (getf r :length)))
          (format t "~&runtime: ~,2f s~%"
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))))))

(eval-when (:execute)
  (when (and (boundp 'sb-ext:*posix-argv*) (>= (length sb-ext:*posix-argv*) 3))
    (main)))
