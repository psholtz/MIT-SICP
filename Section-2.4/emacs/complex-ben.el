;; 
;; From the text, the complex number representation
;; chosen by Ben Bitdiddle (i.e., rectangular coordinates):
;;
(defun square (x) (* x x))

(defun make-from-real-imag (x y) (cons x y))
(defun make-from-mag-ang (r a) (cons (* r (cos a)) (* r (sin a))))
(defun real-part (z) (car z))
(defun imag-part (z) (cdr z))
(defun magnitude (z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defun angle (z) (atan (imag-part z) (real-part z)))


;; 
;; WORKING
;;