;;;; match.lisp — CSS font-family matching over the shipped face set.
;;;;
;;;; A consumer (e.g. a web engine) resolves a CSS `font-family` list plus a
;;;; weight and style into a concrete open-font.  scribe ships the Liberation
;;;; faces — the metric-compatible substitutes browsers use on Linux for
;;;; Arial/Helvetica (Sans), Times (Serif), and Courier (Mono) — so the widths a
;;;; consumer measures match a real browser.
;;;;
;;;; match-font never returns NIL: an unknown family, a missing file, or any
;;;; read error falls back through the family list and ultimately to
;;;; LiberationSans-Regular, so a text path built on it can never crash.
(in-package #:scribe)

;;; ---- the shipped face table -------------------------------------------------
;;; Keyed by (family-key weight-class style) where family-key is one of
;;; :sans :serif :mono :dejavu-sans :dejavu-mono, weight-class is :regular|:bold,
;;; and style is :roman|:italic.  Values are filenames under fonts/ (or the
;;; test corpus for DejaVu).

(defparameter *fonts-dir*
  (asdf:system-relative-pathname "scribe" "fonts/")
  "Directory holding the shipped Liberation faces.")

(defparameter *corpus-dir*
  (asdf:system-relative-pathname "scribe" "inspect/corpus/")
  "Directory holding the test-corpus fonts (DejaVu), used when a consumer asks
for DejaVu explicitly.")

(defparameter *face-files*
  '(((:sans  :regular :roman)  "LiberationSans-Regular.ttf")
    ((:sans  :regular :italic) "LiberationSans-Italic.ttf")
    ((:sans  :bold    :roman)  "LiberationSans-Bold.ttf")
    ((:sans  :bold    :italic) "LiberationSans-BoldItalic.ttf")
    ((:serif :regular :roman)  "LiberationSerif-Regular.ttf")
    ((:serif :regular :italic) "LiberationSerif-Italic.ttf")
    ((:serif :bold    :roman)  "LiberationSerif-Bold.ttf")
    ((:serif :bold    :italic) "LiberationSerif-BoldItalic.ttf")
    ((:mono  :regular :roman)  "LiberationMono-Regular.ttf")
    ((:mono  :regular :italic) "LiberationMono-Italic.ttf")
    ((:mono  :bold    :roman)  "LiberationMono-Bold.ttf")
    ((:mono  :bold    :italic) "LiberationMono-BoldItalic.ttf"))
  "Shipped (family weight style) -> filename under *FONTS-DIR*.")

(defparameter *corpus-face-files*
  '(((:dejavu-sans :regular :roman) "DejaVuSans.ttf")
    ((:dejavu-mono :regular :roman) "DejaVuSansMono.ttf"))
  "DejaVu faces under *CORPUS-DIR* (single weight/style each; bold/italic are
synthesized by the caller's stem darkening / not distinguished here).")

(defvar *font-cache* (make-hash-table :test 'equal)
  "Cache of resolved key -> open-font (or :failed if the file could not be read).")

;;; ---- CSS family -> family-key ----------------------------------------------
;;; Case-insensitive.  Recognized aliases map the common Windows/macOS family
;;; names browsers substitute to the matching Liberation face; generic families
;;; (sans-serif/serif/monospace/system-ui) map to the obvious face.

(defun family-key (name)
  "Map a CSS family NAME (a string) to a face family key, or NIL if unknown."
  (let ((n (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) name))))
    (cond
      ;; sans-serif family and its common metric-compatible aliases
      ((member n '("sans-serif" "arial" "helvetica" "helvetica neue" "verdana"
                   "tahoma" "segoe ui" "segoe" "system-ui" "-apple-system"
                   "blinkmacsystemfont" "roboto" "noto sans" "liberation sans"
                   "arimo" "ui-sans-serif")
               :test #'string=)
       :sans)
      ((and (>= (length n) 5) (string= (subseq n 0 5) "segoe")) :sans)
      ;; serif family and aliases
      ((member n '("serif" "times" "times new roman" "georgia" "garamond"
                   "liberation serif" "tinos" "ui-serif")
               :test #'string=)
       :serif)
      ((and (>= (length n) 5) (string= (subseq n 0 5) "times")) :serif)
      ;; monospace family and aliases
      ((member n '("monospace" "courier" "courier new" "consolas" "menlo"
                   "monaco" "liberation mono" "cousine" "ui-monospace"
                   "sf mono" "dejavu sans mono")
               :test #'string=)
       (if (string= n "dejavu sans mono") :dejavu-mono :mono))
      ;; DejaVu kept available for consumers that ask for it by name
      ((and (>= (length n) 6) (string= (subseq n 0 6) "dejavu"))
       (if (search "mono" n) :dejavu-mono :dejavu-sans))
      (t nil))))

;;; ---- face lookup + lazy open ------------------------------------------------
(defun %read-font-file (path)
  "Read PATH into a byte vector and OPEN-FONT it, or NIL on any error."
  (handler-case
      (let ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                     (let ((v (make-array (file-length s)
                                          :element-type '(unsigned-byte 8))))
                       (read-sequence v s) v))))
        (open-font bytes))
    (error () nil)))

(defun %face-filename+dir (key)
  "Return (values filename directory) for a resolved face KEY (family weight
style), or NIL if the key is not in the shipped/corpus tables."
  (let ((shipped (assoc key *face-files* :test #'equal)))
    (if shipped
        (values (second shipped) *fonts-dir*)
        ;; corpus DejaVu has only a regular roman face; normalize the key
        (let* ((fam (first key))
               (ckey (list fam :regular :roman))
               (c (assoc ckey *corpus-face-files* :test #'equal)))
          (when c (values (second c) *corpus-dir*))))))

(defun %open-face (key)
  "Open (or fetch from cache) the face for resolved KEY, or NIL if unavailable."
  (multiple-value-bind (v present) (gethash key *font-cache*)
    (if present
        (unless (eq v :failed) v)
        (multiple-value-bind (fname dir) (%face-filename+dir key)
          (let ((font (and fname (%read-font-file (merge-pathnames fname dir)))))
            (setf (gethash key *font-cache*) (or font :failed))
            font)))))

;;; ---- the public entry point -------------------------------------------------
(defun match-font (family-list &key (weight 400) (style :normal))
  "Resolve a CSS FAMILY-LIST (a list of family-name strings, or a single string)
plus WEIGHT (100-900) and STYLE (:normal | :italic | :oblique) into an open
font.  Walks the family list, using the first recognized family; falls back to
sans-serif (LiberationSans) for an unknown/empty list.  NEVER returns NIL — a
missing or unreadable face degrades to LiberationSans-Regular, then to any
shipped face.

WEIGHT >= 600 selects the Bold face; STYLE :italic/:oblique selects the Italic
face; both select BoldItalic (for families that have those faces).  DejaVu faces
have a single weight/style, so a bold/italic request on DejaVu returns its
regular face (the caller may synthesize emphasis)."
  (let* ((families (cond ((stringp family-list) (list family-list))
                         ((listp family-list) family-list)
                         (t nil)))
         (fam-key (or (loop for f in families
                            for k = (and (stringp f) (family-key f))
                            when k return k)
                      :sans))
         (weight-class (if (and (integerp weight) (>= weight 600)) :bold :regular))
         (slant (if (member style '(:italic :oblique)) :italic :roman)))
    (or
     ;; exact requested face
     (%open-face (list fam-key weight-class slant))
     ;; drop italic, keep weight
     (%open-face (list fam-key weight-class :roman))
     ;; drop weight, keep italic
     (%open-face (list fam-key :regular slant))
     ;; regular roman of the requested family
     (%open-face (list fam-key :regular :roman))
     ;; ultimate fallback: LiberationSans-Regular, then any shipped face
     (%open-face '(:sans :regular :roman))
     (loop for (k . nil) in *face-files*
           for font = (%open-face k)
           when font return font)
     (error "scribe: match-font found no readable shipped face in ~a" *fonts-dir*))))

;;; ---- metric accessors (for line-height computation) ------------------------
;;; hhea ascent/descent/lineGap in font units; a consumer scales by ppem/upem.

(defun font-ascent (font)
  "hhea typographic ascent in font units (positive)."
  (cdr (assoc "ascent" (parse-hhea font) :test #'string=)))

(defun font-descent (font)
  "hhea typographic descent in font units (negative, per the OpenType spec)."
  (cdr (assoc "descent" (parse-hhea font) :test #'string=)))

(defun font-line-gap (font)
  "hhea line gap in font units (the recommended extra leading between lines)."
  (cdr (assoc "lineGap" (parse-hhea font) :test #'string=)))
