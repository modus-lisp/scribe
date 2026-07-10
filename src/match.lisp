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

;;; ---- script fallback -------------------------------------------------------
;;; match-font resolves the PRIMARY face for a CSS family (Liberation for the
;;; generic Latin families).  Liberation covers Latin/Greek/Cyrillic; for other
;;; scripts a consumer itemizes its text and, for each codepoint the primary face
;;; lacks, asks FALLBACK-FONT-FOR-CODEPOINT for a bundled Noto face covering that
;;; script (Arabic, Hebrew, the Indic scripts, CJK, emoji, ...).  This is Unicode
;;; block -> face itemization, the same model a browser uses; coverage is then
;;; confirmed against the chosen face's cmap so a stray codepoint still resolves.

(defun font-covers-p (font cp)
  "True when FONT has a real (non-.notdef) glyph for Unicode codepoint CP."
  (and font (let ((g (ignore-errors (font-glyph-index font cp)))) (and g (plusp g)))))

(defparameter *fallback-blocks*
  ;; (lo hi filename) — Unicode range -> the bundled Noto face for that script.
  ;; Ranges are disjoint; a linear scan finds the covering face.  CJK ideographs
  ;; and symbols resolve to Noto Sans SC, kana to JP, hangul to KR.
  '((#x0530 #x058F "NotoSansArmenian-Regular.ttf") (#xFB13 #xFB17 "NotoSansArmenian-Regular.ttf")
    (#x0590 #x05FF "NotoSansHebrew-Regular.ttf")   (#xFB1D #xFB4F "NotoSansHebrew-Regular.ttf")
    (#x0600 #x06FF "NotoSansArabic-Regular.ttf")   (#x0750 #x077F "NotoSansArabic-Regular.ttf")
    (#x08A0 #x08FF "NotoSansArabic-Regular.ttf")   (#xFB50 #xFDFF "NotoSansArabic-Regular.ttf")
    (#xFE70 #xFEFF "NotoSansArabic-Regular.ttf")
    (#x0780 #x07BF "NotoSansThaana-Regular.ttf")
    (#x07C0 #x07FF "NotoSansNKo-Regular.ttf")
    (#x0900 #x097F "NotoSansDevanagari-Regular.ttf")
    (#x0980 #x09FF "NotoSansBengali-Regular.ttf")
    (#x0A00 #x0A7F "NotoSansGurmukhi-Regular.ttf")
    (#x0A80 #x0AFF "NotoSansGujarati-Regular.ttf")
    (#x0B00 #x0B7F "NotoSansOriya-Regular.ttf")
    (#x0B80 #x0BFF "NotoSansTamil-Regular.ttf")
    (#x0C00 #x0C7F "NotoSansTelugu-Regular.ttf")
    (#x0C80 #x0CFF "NotoSansKannada-Regular.ttf")
    (#x0D00 #x0D7F "NotoSansMalayalam-Regular.ttf")
    (#x0D80 #x0DFF "NotoSansSinhala-Regular.ttf")
    (#x0E00 #x0E7F "NotoSansThai-Regular.ttf")
    (#x0E80 #x0EFF "NotoSansLao-Regular.ttf")
    (#x0F00 #x0FFF "NotoSerifTibetan-Regular.ttf")
    (#x1000 #x109F "NotoSansMyanmar-Regular.ttf")
    (#x10A0 #x10FF "NotoSansGeorgian-Regular.ttf") (#x1C90 #x1CBF "NotoSansGeorgian-Regular.ttf")
    (#x2D00 #x2D2F "NotoSansGeorgian-Regular.ttf")
    (#x1200 #x139F "NotoSansEthiopic-Regular.ttf") (#x2D80 #x2DDF "NotoSansEthiopic-Regular.ttf")
    (#x13A0 #x13FF "NotoSansCherokee-Regular.ttf") (#xAB70 #xABBF "NotoSansCherokee-Regular.ttf")
    (#x1780 #x17FF "NotoSansKhmer-Regular.ttf")    (#x19E0 #x19FF "NotoSansKhmer-Regular.ttf")
    (#x2C80 #x2CFF "NotoSansCoptic-Regular.ttf")
    (#xA000 #xA4CF "NotoSansYi-Regular.ttf")
    (#xA500 #xA63F "NotoSansVai-Regular.ttf")
    (#x104B0 #x104FF "NotoSansOsage-Regular.ttf")
    (#x1E900 #x1E95F "NotoSansAdlam-Regular.ttf")
    ;; CJK: kana -> JP, hangul -> KR, ideographs/symbols -> SC
    (#x3040 #x30FF "NotoSansJP-Regular.ttf")       (#x31F0 #x31FF "NotoSansJP-Regular.ttf")
    (#x1100 #x11FF "NotoSansKR-Regular.ttf")       (#x3130 #x318F "NotoSansKR-Regular.ttf")
    (#xA960 #xA97F "NotoSansKR-Regular.ttf")       (#xAC00 #xD7FF "NotoSansKR-Regular.ttf")
    (#x2E80 #x2FDF "NotoSansSC-Regular.ttf")       (#x3000 #x303F "NotoSansSC-Regular.ttf")
    (#x3200 #x4DBF "NotoSansSC-Regular.ttf")       (#x4E00 #x9FFF "NotoSansSC-Regular.ttf")
    (#xF900 #xFAFF "NotoSansSC-Regular.ttf")       (#xFF00 #xFFEF "NotoSansSC-Regular.ttf")
    (#x20000 #x2FA1F "NotoSansSC-Regular.ttf")
    ;; emoji & pictographs
    (#x2600 #x27BF "NotoEmoji-Regular.ttf")        (#x2B00 #x2BFF "NotoEmoji-Regular.ttf")
    (#x1F000 #x1FAFF "NotoEmoji-Regular.ttf"))
  "Unicode block -> bundled fallback face filename under *FONTS-DIR*.")

(defparameter *fallback-probe*
  '("NotoSans-Regular.ttf" "NotoSansSC-Regular.ttf" "NotoSansJP-Regular.ttf"
    "NotoSansKR-Regular.ttf" "NotoEmoji-Regular.ttf")
  "Broad-coverage faces probed by cmap when the block table has no entry (or its
face lacks the codepoint) — Noto Sans first for Latin/Greek/Cyrillic extensions.")

(defvar *fallback-cache* (make-hash-table :test 'equal)
  "filename -> open-font (or :failed) for lazily-opened fallback faces.")

(defun %fallback-open (fname)
  "Lazily open (and cache) the fallback face FNAME from *FONTS-DIR*, or NIL."
  (multiple-value-bind (v present) (gethash fname *fallback-cache*)
    (if present
        (unless (eq v :failed) v)
        (let ((font (%read-font-file (merge-pathnames fname *fonts-dir*))))
          (setf (gethash fname *fallback-cache*) (or font :failed))
          font))))

(defun fallback-font-for-codepoint (cp)
  "Return a bundled open-font whose script covers Unicode codepoint CP — via the
block table, confirmed against the face's cmap, else by probing the broad-coverage
faces.  NIL when nothing bundled covers CP (the caller keeps its primary face and
renders .notdef).  Faces are opened lazily and cached, so cost is paid once."
  (let ((fname (loop for (lo hi fn) in *fallback-blocks*
                     when (<= lo cp hi) return fn)))
    (or (and fname (let ((f (%fallback-open fname))) (and (font-covers-p f cp) f)))
        (loop for fn in *fallback-probe*
              for f = (%fallback-open fn)
              when (font-covers-p f cp) return f))))

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
