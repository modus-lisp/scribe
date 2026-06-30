;;;; cff.lisp — CFF (PostScript/Type2) outlines.
;;;;
;;;; Parses the CFF table (INDEX/DICT structures) and interprets Type2
;;;; charstrings into cubic-Bezier contours (font units), feeding the same
;;;; rasterizer as glyf. Non-CID fonts (the common case). Output segments:
;;;;   (:move x y) (:line x y) (:cubic c1x c1y c2x c2y x y)
(in-package #:scribe)

;;; ---- primitive INDEX / DICT readers ----
(defun %cff-uoff (d o size)
  (ecase size (1 (u8 d o)) (2 (u16 d o))
         (3 (logior (ash (u8 d o) 16) (ash (u8 d (+ o 1)) 8) (u8 d (+ o 2))))
         (4 (u32 d o))))

(defun read-index (d off)
  "Parse a CFF INDEX at OFF. Returns (values entries next-offset) where ENTRIES
   is a vector of (start . end) absolute byte ranges into D."
  (let ((count (u16 d off)))
    (when (zerop count) (return-from read-index (values #() (+ off 2))))
    (let* ((osz (u8 d (+ off 2)))
           (base (+ off 3))
           (data (+ base (* (1+ count) osz) -1))
           (entries (make-array count)))
      (dotimes (i count)
        (setf (aref entries i)
              (cons (+ data (%cff-uoff d (+ base (* i osz)) osz))
                    (+ data (%cff-uoff d (+ base (* (1+ i) osz)) osz)))))
      (values entries (+ data (%cff-uoff d (+ base (* count osz)) osz))))))

(defun parse-dict (d start end)
  "Parse a CFF DICT in [START,END) -> alist (operator . operand-list)."
  (let ((p start) (ops '()) (out '()))
    (loop while (< p end) do
      (let ((b (u8 d p)))
        (cond
          ((<= 32 b 246) (push (- b 139) ops) (incf p))
          ((<= 247 b 250) (push (+ (* (- b 247) 256) (u8 d (1+ p)) 108) ops) (incf p 2))
          ((<= 251 b 254) (push (- (* (- (- b 251)) 256) (u8 d (1+ p)) 108) ops) (incf p 2))
          ((= b 28) (push (s16 d (1+ p)) ops) (incf p 3))
          ((= b 29) (push (let ((v (u32 d (1+ p)))) (if (>= v #x80000000) (- v #x100000000) v)) ops) (incf p 5))
          ((= b 30)                                        ; real (BCD)
           (incf p) (let ((s (make-string-output-stream)) (done nil))
                      (loop until done do
                        (let ((by (u8 d p))) (incf p)
                          (dolist (nib (list (ash by -4) (logand by 15)))
                            (case nib (#xf (setf done t) (return))
                              (#xa (write-char #\. s)) (#xb (write-char #\E s))
                              (#xc (write-string "E-" s)) (#xe (write-char #\- s))
                              (#xd nil) (t (write-char (digit-char nib) s))))))
                      (push (let ((*read-default-float-format* 'double-float))
                              (ignore-errors (read-from-string (get-output-stream-string s)))) ops)))
          (t (let ((op (if (= b 12) (prog1 (+ 1200 (u8 d (1+ p))) (incf p 2)) (prog1 b (incf p)))))
               (push (cons op (nreverse ops)) out) (setf ops nil))))))
    (nreverse out)))

(defun dict-val (dict key &optional default)
  (let ((e (assoc key dict))) (if e (cdr e) default)))

;;; ---- CFF font state, parsed once and cached on the font ----
(defstruct cff
  data charstrings gsubrs lsubrs gbias lbias
  (nominal-width 0) (default-width 0))

(defun subr-bias (n) (cond ((< n 1240) 107) ((< n 33900) 1131) (t 32768)))

(defun load-cff (font)
  (let* ((d (font-data font)) (base (req-table font "CFF ")))
    (multiple-value-bind (_ p1) (read-index d (+ base (u8 d (+ base 2)))) ; Name INDEX
      (declare (ignore _))
      (multiple-value-bind (topdicts p2) (read-index d p1)               ; Top DICT INDEX
        (multiple-value-bind (__ p3) (read-index d p2)                   ; String INDEX
          (declare (ignore __))
          (multiple-value-bind (gsubrs p4) (read-index d p3)             ; Global Subr INDEX
            (declare (ignore p4))
            (let* ((td (parse-dict d (car (aref topdicts 0)) (cdr (aref topdicts 0))))
                   (cs-off (first (dict-val td 17)))
                   (priv (dict-val td 18))
                   (lsubrs #()) (nomw 0) (defw 0))
              (when priv
                (let* ((psz (first priv)) (poff (second priv))
                       (pd (parse-dict d (+ base poff) (+ base poff psz))))
                  (setf nomw (or (first (dict-val pd 21)) 0)
                        defw (or (first (dict-val pd 20)) 0))
                  (let ((lso (first (dict-val pd 19))))
                    (when lso (setf lsubrs (read-index d (+ base poff lso)))))))
              (multiple-value-bind (cstrings) (read-index d (+ base cs-off))
                (make-cff :data d :charstrings cstrings :gsubrs gsubrs :lsubrs lsubrs
                          :gbias (subr-bias (length gsubrs)) :lbias (subr-bias (length lsubrs))
                          :nominal-width nomw :default-width defw)))))))))

(defun font-cff (font)
  (or (font-%cff font) (setf (font-%cff font) (load-cff font))))

;;; ---- Type2 charstring interpreter ----
(defun cff-glyph-outline (font gid)
  (let* ((c (font-cff font)) (d (cff-data c))
         (cs (cff-charstrings c)))
    (when (>= gid (length cs)) (return-from cff-glyph-outline nil))
    (let ((st (make-array 48 :adjustable t :fill-pointer 0))
          (x 0d0) (y 0d0) (contours '()) (cur '())
          (nstems 0) (width-done nil) (open nil))
      (labels ((push* (v) (vector-push-extend (float v 1d0) st))
               (clear () (setf (fill-pointer st) 0))
               (nargs () (fill-pointer st))
               (take-width-parity (expected)
                 (unless width-done
                   (when (> (nargs) expected)
                     ;; drop the bottom-most (width) arg
                     (loop for i from 1 below (fill-pointer st)
                           do (setf (aref st (1- i)) (aref st i)))
                     (decf (fill-pointer st)))
                   (setf width-done t)))
               (take-width-even ()
                 (unless width-done
                   (when (oddp (nargs))
                     (loop for i from 1 below (fill-pointer st)
                           do (setf (aref st (1- i)) (aref st i)))
                     (decf (fill-pointer st)))
                   (setf width-done t)))
               (moveto (nx ny)
                 (when open (push (nreverse cur) contours))
                 (setf x nx y ny cur (list (list :move x y)) open t))
               (lineto (nx ny) (push (list :line nx ny) cur) (setf x nx y ny))
               (curveto (c1x c1y c2x c2y nx ny)
                 (push (list :cubic c1x c1y c2x c2y nx ny) cur) (setf x nx y ny))
               (run (start end)
                 (let ((p start))
                   (loop while (< p end) do
                     (let ((b (u8 d p)))
                       (cond
                         ((>= b 32)
                          (cond ((<= b 246) (push* (- b 139)) (incf p))
                                ((<= b 250) (push* (+ (* (- b 247) 256) (u8 d (1+ p)) 108)) (incf p 2))
                                ((<= b 254) (push* (- (* (- (- b 251)) 256) (u8 d (1+ p)) 108)) (incf p 2))
                                (t (push* (/ (let ((v (u32 d (1+ p)))) (if (>= v #x80000000) (- v #x100000000) v)) 65536d0)) (incf p 5))))
                         ((= b 28) (push* (s16 d (1+ p))) (incf p 3))
                         (t (incf p)
                            (case b
                              ((1 3 18 23)                  ; h/v stem(hm)
                               (take-width-even) (incf nstems (floor (nargs) 2)) (clear))
                              ((19 20)                      ; hintmask/cntrmask
                               (take-width-even) (incf nstems (floor (nargs) 2)) (clear)
                               (incf p (ceiling nstems 8)))
                              (21 (take-width-parity 2) (moveto (+ x (aref st 0)) (+ y (aref st 1))) (clear))
                              (22 (take-width-parity 1) (moveto (+ x (aref st 0)) y) (clear))
                              (4  (take-width-parity 1) (moveto x (+ y (aref st 0))) (clear))
                              (5  (loop for i from 0 below (1- (nargs)) by 2  ; rlineto
                                        do (lineto (+ x (aref st i)) (+ y (aref st (1+ i))))) (clear))
                              (6  (do-alt-line t) (clear))   ; hlineto
                              (7  (do-alt-line nil) (clear)) ; vlineto
                              (8  (loop for i from 0 below (- (nargs) 5) by 6 ; rrcurveto
                                        do (rrc i)) (clear))
                              (24 (let ((n (nargs)))         ; rcurveline
                                    (loop for i from 0 below (- n 2) by 6 do (rrc i))
                                    (let ((i (- n 2))) (lineto (+ x (aref st i)) (+ y (aref st (1+ i)))))) (clear))
                              (25 (let ((n (nargs)))         ; rlinecurve
                                    (loop for i from 0 below (- n 6) by 2
                                          do (lineto (+ x (aref st i)) (+ y (aref st (1+ i)))))
                                    (rrc (- n 6))) (clear))
                              (26 (vvc) (clear))             ; vvcurveto
                              (27 (hhc) (clear))             ; hhcurveto
                              (30 (vhc nil) (clear))         ; vhcurveto
                              (31 (vhc t) (clear))           ; hvcurveto
                              (10 (let ((idx (+ (round (vector-pop st)) (cff-lbias c)))) ; callsubr
                                    (let ((e (aref (cff-lsubrs c) idx))) (run (car e) (cdr e)))))
                              (29 (let ((idx (+ (round (vector-pop st)) (cff-gbias c)))) ; callgsubr
                                    (let ((e (aref (cff-gsubrs c) idx))) (run (car e) (cdr e)))))
                              (11 (return-from run))         ; return
                              (14 (take-width-parity 0)      ; endchar
                                  (when open (push (nreverse cur) contours)) (setf open nil)
                                  (return-from cff-glyph-outline (nreverse contours)))
                              (12 (let ((op2 (u8 d p))) (incf p)  ; escape: flex family
                                    (case op2 (34 (hflex)) (35 (flex)) (36 (hflex1)) (37 (flex1)))
                                    (clear)))
                              (t nil))))))))
               ;; --- curve helpers (operate on the stack st) ---
               (rrc (i) (let* ((c1x (+ x (aref st i))) (c1y (+ y (aref st (+ i 1))))
                               (c2x (+ c1x (aref st (+ i 2)))) (c2y (+ c1y (aref st (+ i 3))))
                               (ex (+ c2x (aref st (+ i 4)))) (ey (+ c2y (aref st (+ i 5)))))
                          (curveto c1x c1y c2x c2y ex ey)))
               (do-alt-line (horiz)
                 (loop for i from 0 below (nargs) do
                   (if horiz (lineto (+ x (aref st i)) y) (lineto x (+ y (aref st i))))
                   (setf horiz (not horiz))))
               (vvc ()
                 (let* ((n (nargs)) (i 0) (dx1 0d0))
                   (when (oddp n) (setf dx1 (aref st 0) i 1))
                   (loop while (< i n) do
                     (let* ((c1x (+ x dx1)) (c1y (+ y (aref st i)))
                            (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                            (ex c2x) (ey (+ c2y (aref st (+ i 3)))))
                       (curveto c1x c1y c2x c2y ex ey) (setf dx1 0d0) (incf i 4)))))
               (hhc ()
                 (let* ((n (nargs)) (i 0) (dy1 0d0))
                   (when (oddp n) (setf dy1 (aref st 0) i 1))
                   (loop while (< i n) do
                     (let* ((c1x (+ x (aref st i))) (c1y (+ y dy1))
                            (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                            (ex (+ c2x (aref st (+ i 3)))) (ey c2y))
                       (curveto c1x c1y c2x c2y ex ey) (setf dy1 0d0) (incf i 4)))))
               ;; --- flex family: each emits two cubic curves (s = stack) ---
               (flex ()    ; 12 args used: dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 dx6 dy6
                 (let* ((c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy (+ c2y (aref st 5))))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 6))) (c4y (+ jy (aref st 7)))
                          (c5x (+ c4x (aref st 8))) (c5y (+ c4y (aref st 9)))
                          (ex (+ c5x (aref st 10))) (ey (+ c5y (aref st 11))))
                     (curveto c4x c4y c5x c5y ex ey))))
               (hflex ()   ; dx1 dx2 dy2 dx3 dx4 dx5 dx6
                 (let* ((sy y) (c1x (+ x (aref st 0))) (c1y y)
                        (c2x (+ c1x (aref st 1))) (c2y (+ c1y (aref st 2)))
                        (jx (+ c2x (aref st 3))) (jy c2y))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 4))) (c4y jy)
                          (c5x (+ c4x (aref st 5))) (c5y sy)
                          (ex (+ c5x (aref st 6))) (ey sy))
                     (curveto c4x c4y c5x c5y ex ey))))
               (hflex1 () ; dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6
                 (let* ((sy y) (c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy c2y))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 5))) (c4y jy)
                          (c5x (+ c4x (aref st 6))) (c5y (+ c4y (aref st 7)))
                          (ex (+ c5x (aref st 8))) (ey sy))
                     (curveto c4x c4y c5x c5y ex ey))))
               (flex1 ()  ; dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 d6
                 (let* ((sx x) (sy y)
                        (dxs (+ (aref st 0) (aref st 2) (aref st 4) (aref st 6) (aref st 8)))
                        (dys (+ (aref st 1) (aref st 3) (aref st 5) (aref st 7) (aref st 9)))
                        (c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy (+ c2y (aref st 5))))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 6))) (c4y (+ jy (aref st 7)))
                          (c5x (+ c4x (aref st 8))) (c5y (+ c4y (aref st 9))) ex ey)
                     (if (> (abs dxs) (abs dys))
                         (setf ex (+ c5x (aref st 10)) ey sy)
                         (setf ex sx ey (+ c5y (aref st 10))))
                     (curveto c4x c4y c5x c5y ex ey))))
               (vhc (start-horiz)
                 (let ((n (nargs)) (i 0) (horiz start-horiz))
                   (loop while (>= (- n i) 4) do
                     (let ((last5 (= (- n i) 5)))
                       (if horiz
                           (let* ((c1x (+ x (aref st i))) (c1y y)
                                  (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                                  (ey (+ c2y (aref st (+ i 3))))
                                  (ex (if last5 (+ c2x (aref st (+ i 4))) c2x)))
                             (curveto c1x c1y c2x c2y ex ey))
                           (let* ((c1x x) (c1y (+ y (aref st i)))
                                  (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                                  (ex (+ c2x (aref st (+ i 3))))
                                  (ey (if last5 (+ c2y (aref st (+ i 4))) c2y)))
                             (curveto c1x c1y c2x c2y ex ey)))
                       (setf horiz (not horiz)) (incf i 4)))
                   (clear))))
        (let ((e (aref cs gid))) (run (car e) (cdr e)))
        (when open (push (nreverse cur) contours))
        (nreverse contours)))))
