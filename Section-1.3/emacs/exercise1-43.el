;;
;; Exercise 1.43
;;
;; If f is a numerical function and n is a positive integer, then we can form the nth repeated application
;; of f, which is defined to be the function whose value at x is f(f(f(...(f(x))...))). For example, if 
;; f is the function x --> x+1, then the nth repeated application of f is the function x -->  x+n. If f is 
;; the operation of squaring a number, then the nth repeated application of f is the function that raises
;; its argument to the 2^n-th power. Write a procedure that takes as inputs a procedure that computes f
;; and a positive integer n and returns the proceudre that computes the nth repeated application of f.
;; Your procedure should be able to be used as follows:
;;
;; ((repeated square 2) 5)
;; ==> 625
;; 
;; Hint: you may find it convenient to use "compose" from exercise 1.42.
;;

;;
;; We need to use lexical-scoping:
;;
(require 'cl)

;;
;; Give the "compose" procedure from exercise 1.42:
;;
(defun compose (f g)
  (lexical-let ((foo f)
		(goo g))
	       (lambda (x)
		 (funcall foo (funcall goo x)))))

;;
;; Definition of the "repeated" procedure:
;;
(defun repeated(f n)
  (lexical-let ((foo f))
    (defun repeated-iter (g c)
      (lexical-let ((goo g))
		   (cond ((>= c n)goo)
			       (t
				       (repeated-iter (compose goo foo) (+ c 1))))))
    (repeated-iter foo 1)))

;;
;; Use cases:
;;
(defun square (n) (* n n))

(funcall (repeated #'square 2) 5)
;; ==> 625