;;
;; Alyssa's representation of complex numbers.
;; She has chosen to use the polar representation.
;;
(defun square (x) (* x x))

(defun make-from-real-imag (x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
(defun make-from-mag-ang (r a) (cons r a))
(defun magnitude (z) (car z))
(defun angle (z) (cdr z))
(defun real-part (z) (* (magnitude z) (cos (angle z))))
(defun imag-real (z) (* (magnitude z) (sin (angle z))))