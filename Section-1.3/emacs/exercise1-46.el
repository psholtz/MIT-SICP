;;
;; Exercise 1.46
;;
;; Several of the numerical methods described in this chapter are instances of an extremely
;; general computational strategy known as iterative improvement. Iterative improvement says that, 
;; to compute something, we start with an initial guess for the answer, test if the guess 
;; is good enough, and otherwise improve the guess and continue the process using the improved
;; gues as the new guess. Write a procedure "iterative-improve" that takes two procedures as 
;; arguments: a method for telling whether a guess is good enough and a method for improving
;; a guess. "Iterative-improve" should return as its value a procedure that takes a guess
;; as argument and keeps improving the guess until it is good enough. Rewrite the "sqrt" procedure
;; of section 1.1.7 and the "fixed-point" procedure of section 1.3.3 in terms of "iterative-improve".
;;

;;
;; Use lexical binding
;;
(require 'cl)

;;
;; The trick is that we have to be able to "iterate" calls to the procedure 
;; until we arrive at an answer that is "good enough". For this reason, we 
;; define an "inner" procedure that we can invoke repeatedly until we arrive
;; at a "good enough" answer.
;;
(defun iterative-improvement (good-enough? improve)
  (lexical-let ((goo good-enough?)
		(ioo improve))
	       (lambda (first-guess)
		 (defun iteration (guess)
		   (if (funcall goo guess)
		       guess
		     (iteration (funcall ioo guess))))
		 (iteration first-guess))))

;;
;; Define "sqrt" in terms of "iterative-improvement":
;;
(defun sqrt (x)
  (lexical-let ((x1 x))
	       (setq tolerance 0.00001)
	       (defun average (a b) (/ (+ a b) 2.0))
	       (defun good-enough? (guess)
		 (< (abs (- (square guess) x1)) tolerance))
	       (defun improve (guess)
		 (average guess (/ x1 guess)))
	       (funcall (iterative-improvement #'good-enough? #'improve) 1.)))

(defun square (x) (* x x))

;;
;; Run some unit tests:
;;
(sqrt 2)
;; ==> 1.4142156862745097

(sqrt 3)
;; ==>1.7320508100147274

(sqrt 5)
;; ==> 2.2360688956433634

(sqrt 10)
;; ==> 3.162277665175675

;;
;; Define the "fixed-point" procedure in terms of "iterative-improvement":
;;
(defun fixed-point (f x)
  (lexical-let ((foo f))
	       (setq tolerance 0.00001)
	       (defun close-enough? (guess)
		 (< (abs (- guess (funcall foo guess))) tolerance))
	       (defun next (x) (funcall foo x))
	       (funcall (iterative-improvement #'close-enough? #'next) 1.0)))

;;
;; Run some unit tests:
;;
(fixed-point #'cos 1.0)
;; ==> 0.7390893414033928

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;; ==> 1.2587228743052672

(defun average (a b) (/ (+ a b) 2.0))

(defun average-damp (f)
  (lexical-let ((foo f))
	       (lambda (x) (average x (funcall foo x)))))

;; 
;; Define "sqrt" and "cube-root" procedures in terms of fixed-point:
;;
(defun sqrt (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (lambda (y) (/ x1 y))) 1.0)))

(sqrt 2)
;; ==> 1.4142156862745097

(sqrt 3)
;; ==> 1.7320508100147274

(defun cube-root (x)
  (lexical-let ((x1 x))
	       (fixed-point (average-damp (lambda (y) (/ x1 (square y)))) 1.0)))

(cube-root 2)
;; ==> 1.2599166768842038

(cube-root 3)
;; ==> 1.4422451140553103