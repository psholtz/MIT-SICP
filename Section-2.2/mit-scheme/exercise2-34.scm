;;
;; Exercise 2.34
;;
;; Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. 
;; We evaluate the polynomial 
;;
;; a(n)x^n + a(n-1)x^(n-1) + .. + a(1)x + a(0)
;;
;; using a well-known algorithm called Horner's rule, which structures the computation as 
;;
;; (.. (a(n)x + a(n-1))x + ... + a(1))x + a(0)
;;
;; In other words, we start with a(n), multiply by x, add a(n-1), multiply by x, and so on,
;; until we reach a(0). Fill in the following template to produce a procedure that evaluates
;; a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged
;; in a sequence, from a(0) through a(n).
;;
;; (define (horner-eval x coefficient-sequence)
;;  (accumulate (lambda (this-coeff higher-terms) <??>)
;;              0
;;              coefficient-sequence))
;;
;; For example, to compute 1 + 3x + 5x^3 + x^5 at x=2, you would evaluate:
;;
;; (horner-eval 2 (list 1 3 0 5 0 1))
;;

;;
;; Define the "accumulate" procedure:
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;
;; Define the "horner-eval" procedure:
;;
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;;
;; Run some unit tests:
;;

;;
;; Model f(x) = 1
;;
(horner-eval 0 (list 1))
;; ==> 1
(horner-eval 1 (list 1))
;; ==> 1
(horner-eval 2 (list 1))
;; ==> 1
(horner-eval 10 (list 1))
;; ==> 1
(horner-eval -1 (list 1))
;; ==> 1

;;
;; Model f(x) = 0
;;
(horner-eval 0 (list 0))
;; ==> 0
(horner-eval 1 (list 0))
;; ==> 0
(horner-eval 2 (list 0))
;; ==> 0
(horner-eval 10 (list 0))
;; ==> 0
(horner-eval -1 (list 0))
;; ==> 0

;;
;; Model f(x) = 10
;;
(horner-eval 0 (list 10))
;; ==> 10
(horner-eval 1 (list 10))
;; ==> 10
(horner-eval 2 (list 10))
;; ==> 10
(horner-eval 10 (list 10))
;; ==> 10
(horner-eval -1 (list 10))
;; ==> 10


;;
;; Model f(x) = 2x+1
;;
(horner-eval -1 (list 1 2))
;; ==> -1
(horner-eval 0 (list 1 2))
;; ==> 1
(horner-eval 1 (list 1 2))
;; ==> 3
(horner-eval 2 (list 1 2))
;; ==> 5 
(horner-eval 10 (list 1 2))
;; ==> 21

;;
;; Model f(x) = x^2 + 2x + 1
;;
(horner-eval -1 (list 1 2 1))
;; ==> 0
(horner-eval 0 (list 1 2 1))
;; ==> 1
(horner-eval 1 (list 1 2 1))
;; ==> 4
(horner-eval 2 (list 1 2 1))
;; ==> 9
(horner-eval 10 (list 1 2 1))
;; ==> 121

;;
;; Model f(x) = 1 + 3x + 5x^3 + x^5, as in the text
;;
(horner-eval -1 (list 1 3 0 5 0 1))
;; ==> -8
(horner-eval 0 (list 1 3 0 5 0 1))
;; ==> 1
(horner-eval 1 (list 1 3 0 5 0 1))
;; ==> 10
(horner-eval 2 (list 1 3 0 5 0 1))
;; ==> 79
(horner-eval 10 (list 1 3 0 5 0 1))
;; ==> 105,031