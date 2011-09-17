;;
;; Exercise 1.35
;;
;; Show that the golden ratio phi (section 1.2.2) is a fixed point of the transformation x -> 1 + 1/x, 
;; and use this fact to compute phi by means of the fixed-point procedure.
;;

;;
;; Define the "fixed-point" procedure:
;;
(defun fixed-point (f first-guess)
  (setq tolerance 0.00001)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
	  next
	(try next))))
  (try first-guess))

;;
;; Define "phi" as a fixed point:
;;
(setq phi (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

phi
;; ==> 1.61803279