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


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))
