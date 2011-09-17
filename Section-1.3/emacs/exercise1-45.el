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
(setq max-lisp-eval-depth 1000)
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
	       (fixed-point (average-damp (average-damp (lambda (y) (/ x1 (n-th-power y 3))))) 1.0)))

(defun fifth-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (average-damp (lambda (y) (/ x1 (n-th-power y 4))))) 1.0)))

(defun sixth-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (average-damp (lambda (y) (/ x1 (n-th-power y 5))))) 1.0)))

(defun seventh-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (average-damp (lambda (y) (/ x1 (n-th-power y 6))))) 1.0)))

;;
;; These procedures require three calls to "average-damp":
;;
(defun eighth-root (x)
  (lexical-let ((x1 x))
	       (fixed-point
		(average-damp 
		 (average-damp
		  (average-damp (lambda (y) (/ x1 (n-th-power y 7)))))) 1.0)))

;;
;; ...
;;

(defun fifteenth-root (x)
  (lexical-let ((x1 x))
	       (fixed-point
		(average-damp 
		 (average-damp
		  (average-damp (lambda (y) (/ x1 (n-th-power y 14)))))) 1.0)))

;; 
;; The following procedure requires four calls to "average-damp":
;;
(defun sixteenth-root (x)
  (lexical-let ((x1 x))
	       (fixed-point 
		(average-damp
		 (average-damp
		  (average-damp
		   (average-damp (lambda (y) (/ x1 (n-th-power y 15))))))) 1.0)))

;;
;; A pattern suggests itself:
;; 
;; n = 2 or n = 3 ==> apply "average-damp" 1x
;; n = 4 or n = 5 or n = 6 or n = 7 ==> apply "average-damp" 2x
;; n = 8 or n = 9 or n = 10 or n = 11 
;; or n = 12 or n = 13 or n = 14 or n = 15 ==> apply "average-damp" 3x
;; n = 16 ... ==> apply "average-damp" 4x
;;
;; The function of n we are looking for, to give the number of times 
;; we should apply "average-damp", is given by:
;;
;; (floor (/ (log n) (log 2)))
;;
(= 1 (floor (/ (log 2) (log 2))))
(= 1 (floor (/ (log 3) (log 2))))
(= 2 (floor (/ (log 4) (log 2))))
(= 2 (floor (/ (log 5) (log 2))))
(= 2 (floor (/ (log 6) (log 2))))
(= 2 (floor (/ (log 7) (log 2))))
(= 3 (floor (/ (log 8) (log 2))))
(= 4 (floor (/ (log 16) (log 2))))

;;
;; Let's define the "repeated" procedure from the previous exercises, 
;; so we can use that in our procedure definition:
;;
(defun compose (f g)
  (lexical-let ((foo f)
		(goo f))
	       (lambda (x)
		 (funcall foo (funcall goo x)))))

(defun repeated (f n)
  (lexical-let ((foo f))
	       (defun repeated-iter (g c)
		 (lexical-let ((goo g))
			      (cond ((>= c n) goo)
				    (t
				     (repeated-iter (compose goo foo) (+ c 1))))))
	       (repeated-iter foo 1)))

;;
;; Finally define the n-th-root procedure that we are seeking:
;;
(defun n-th-root (x n)
  (lexical-let ((k (floor (/ (log n) (log 2))))
		(x1 x)
		(n1 n))
    (fixed-point (funcall (repeated #'average-damp k)
		  (lambda (y) (/ x1 (n-th-power y (- n1 1)))))
		 1.0)))

;;
;; Let's run some unit tests:
;;
(n-th-root 2 2)
;; ==> 1.4142135623746899

(n-th-root 3 2)
;; ==> 1.7320508075688772

(n-th-root 81 2)
;; ==> 9.0

(n-th-root 3 5)
;; ==> 1.2457295735853005

(n-th-root 2 10)
;; ==> 1.071770200377089