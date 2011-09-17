;;
;; Define a procedure "cubic" that can be used together with the "newtons-method" procedure in expressions of the form
;;
;; (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;;

;;
;; Must include "common lisp" to get the lexical scoping.
;;
(require 'cl)

;;
;; Let's increase the buffers as well.
;;
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;;
;; Let's define all the functions we need to make this example work.
;;
;; First let's define some supporting polynomial procedures:
;;
(defun cube (n) (* n n n))
(defun square (n) (* n n))

;;
;; Now define the fixed-point procedure:
;;
(defun fixed-point (f first-guess)
  (setq tolerance 0.00001)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess number)
    (let ((next (funcall f guess)))
      (newline)
      (princ "Guess number ")
      (princ number)
      (princ ": ")
      (princ guess)
      (cond ((close-enough? guess next)
	     (newline)
	     next)
	    (t
	     (try next (+ number 1))))))
  (try first-guess 1))

;;
;; Next define the procedures we need to support Newton's Method:
;;
(defun average (x y) (/ (+ x y) 2.0))

;;
;; Higher-order procedures like "average-damp" are harder to implement
;; in emacs, since emacs does not have native support for lexical binding.
;; We include (require 'cl) and perform a lexical-let operation to mimic 
;; such support.
;;
(defun average-damp (f)
  (lexical-let ((foo f))
	       (lambda (x)
		 (average x (funcall foo x)))))

;;
;; Once we have the average-damp procedure, we must still invoke it 
;; using the "funcall" procedure. That is:
;;
;; (= (funcall (average-damp #'square) 10) 55.0)
;;

;;
;; Similiarly with the higher order procedure "deriv", we must
;; use lexical scoping to bind the function variable.
;;
(defun deriv (g)
  (lexical-let ((goo g)
		(dx 0.00001))
	       (lambda (x)
		 (/ (- (funcall goo (+ x dx)) (funcall goo x))
		    dx))))

(defun newtons-method (g guess)
  (defun newton-transform ()
    (lambda (x)
      (- (/ (funcall g x) (funcall (deriv g) x)))))
  (fixed-point (newton-transform) guess))

;;
;; Finally let's define the cubic procedure.
;;
;; As with some of the other procedures, we must again use 
;; lexical scoping in our definition of the procedure, 
;; and we must invoke the procedure using "funcall", that is:
;;
;; (= (funcall (cubic 1 2 3) 1) 7)
;;
(defun cubic (a b c)
  (lexical-let ((a1 a)
		(b1 b)
		(c1 c))
	       (lambda (x)
		 (+ (cube x)
		    (* a1 (square x))
		    (* b1 x)
		    c1))))

;;
;; Let's run some simple tests.
;; 
;; The root of x^3 is zero. Let's see if our procedure works for this use case:
;;
(newtons-method (cubic 0 0 0) 1.0)
;; ==> 2.7755295616632356e-06

;;
;; Several other use cases are defined in the scheme implementation, but 
;; emac is not able to recurse deeply enough to establish them.
;;
