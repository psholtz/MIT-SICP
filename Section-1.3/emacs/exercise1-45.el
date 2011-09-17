;;
;; Exercise 1.45
;;
;; We saw in section 1.3.3 that attempting to compute square roots by naively 
;; finding a fixed point of y --> x/y does not converge, and that this can be
;; fixed by average damping. The same method works for finding cube roots as 
;; fixed points of the average-damped y --> x/y^2. Unfortunately, the process 
;; does not work for fourth roots -- a single average damp is not enough to make
;; a fixed-point search for y --> x / y^3 converge. On the other hand, if we 
;; average damp twice (i.e., use the average damp of the average damp of y --> x/y^3)
;; the fixed-point search does converge. Do some experiments to determine how
;; many average damps are required to compute nth roots as a fixed-point search
;; based upon repeated average damping of y --> x /y^(n-1). Use this to implement
;; a simple procedure for computing n-th roots using "fixed-point", "average-damp"
;; and the "repeated" procedure of exercise 1.43. Assume that any arithmetic operations
;; you need are available as primitives.
;;

;;
;; Require lexical scoping and increase buffer sizes
;;
(require 'cl)
(setq max-lisp-eval-depth 100)
(setq max-specpdl-size 1800)

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
;; Define the "average damping" procedures:
;;
(defun average (x y) (/ (+ x y) 2.0))

(defun average-damp (f)
  (lexical-let ((foo f))
	       (lambda (x)
		 (average x (funcall foo x)))))

;;
;; Define a utility procedure that allows us to raise the number x to the n-th power:
;;
(defun n-th-power (x n)
  (defun n-th-power-iter (c v)
    (if (= c n)
	v
      (n-th-power-iter (+ c 1) (* v x))))
  (n-th-power-iter 0 1))

;;
;; Run some unit tests of "n-th-power":
;;
(n-th-power 2 0)
;; ==> 1

(n-th-power 2 1)
;; ==> 2

(n-th-power 2 2)
;; ==> 4

(n-th-power 2 10)
;; ==> 1024

(n-th-power 3 2)
;; ==> 9

(n-th-power 3 4)
;; ==> 81

;; 
;; These procedures require one call to "average-damp":
;;
(defun square-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (lambda (y) (/ x1 (n-th-power y 1)))) 1.0)))

(defun cube-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (lambda (y) (/ x1 (n-th-power y 2)))) 1.0)))

;;
;; These procedures require two calls to "average-damp":
;;
(defun fourth-root (x)
  (lexical-let ((x1 x))
	       (lexical-let ((v1 (average-damp (lambda (y) (/ x1 (n-th-power y 3))))))
			    v1)))

;; [[ WORKING ]]

