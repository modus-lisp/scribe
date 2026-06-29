;;;; gamma-demo.lisp — make the most important invisible lever visible.
;;;;
;;;; Renders black-on-white coverage the WRONG way (sRGB-space blend) and the
;;;; RIGHT way (linear-light blend), plus the classic stripe proof, so you can
;;;; see why anti-aliased text must be composited in linear light.
;;;;
;;;;   sbcl --script demo/gamma-demo.lisp   ->  /tmp/scribe-gamma.png
(let ((here (directory-namestring *load-truename*)))
  (load (merge-pathnames "../src/packages.lisp" here))
  (load (merge-pathnames "../src/blend.lisp" here)))

(in-package #:scribe)

(defun naive-blend (cv x y coverage fg)
  "WRONG: lerp in sRGB space (what most homemade rasterizers do)."
  (when (and (>= x 0) (>= y 0) (< x (canvas-width cv)) (< y (canvas-height cv)))
    (let* ((px (canvas-pixels cv)) (i (* 3 (+ (* y (canvas-width cv)) x)))
           (a coverage) (ia (- 1d0 a)))
      (flet ((mix (bg f) (min 255 (max 0 (round (+ (* ia bg) (* a f)))))))
        (setf (aref px i)       (mix (aref px i) (first fg))
              (aref px (+ i 1)) (mix (aref px (+ i 1)) (second fg))
              (aref px (+ i 2)) (mix (aref px (+ i 2)) (third fg)))))))

(defun solid (cv x0 y0 w h r g b)
  (loop for y from y0 below (+ y0 h) do
    (loop for x from x0 below (+ x0 w) do
      (let ((i (* 3 (+ (* y (canvas-width cv)) x))) (px (canvas-pixels cv)))
        (when (and (< x (canvas-width cv)) (< y (canvas-height cv)))
          (setf (aref px i) r (aref px (+ i 1)) g (aref px (+ i 2)) b))))))

(let* ((w 900) (h 520) (cv (make-canvas w h '(255 255 255)))
       (black '(0 0 0)) (m 30))
  ;; Two black-on-white coverage ramps, coverage = x/width.
  ;; Top: naive sRGB blend.  Bottom: linear-light blend.
  (loop for x from m below (- w m) do
    (let ((covf (/ (float (- x m) 1d0) (float (- w m m) 1d0))))
      (loop for y from 30 below 130 do (naive-blend cv x y covf black))
      (loop for y from 160 below 260 do (blend-coverage cv x y covf black))))
  ;; The stripe proof (view at 1:1!): center = 1px black/white lines (physical
  ;; 50% coverage); left swatch = sRGB 128 (naive "half"); right = sRGB 188
  ;; (linear-correct half).  The stripes match the RIGHT swatch, not the left.
  (let ((y0 320) (hh 150) (third (floor (- w m m) 3)))
    (solid cv m y0 third hh 128 128 128)                          ; naive half-gray
    (solid cv (+ m (* 2 third)) y0 third hh 188 188 188)          ; linear half-gray
    (loop for y from y0 below (+ y0 hh) by 2 do                   ; 1px stripes = 50%
      (solid cv (+ m third) y third 1 0 0 0)))
  (write-png cv "/tmp/scribe-gamma.png")
  (format t "wrote /tmp/scribe-gamma.png~%")
  (format t "  rows 30-130  : coverage ramp, NAIVE sRGB blend (too dark in mids)~%")
  (format t "  rows 160-260 : coverage ramp, LINEAR-light blend (correct)~%")
  (format t "  rows 320-470 : stripe proof — 1px stripes match the 188 swatch (right), not 128 (left)~%"))
