;;
;; Exercise 2.34
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