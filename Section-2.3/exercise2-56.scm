;;
;; Exercise 2.56
;;

;;
;; Load the library with the necessary procedures.
;;
(load "differentiation.scm")

;; 
;; Redefine the "deriv" procedure to handle exponentiation.
;;
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (exponent exp)
	  (make-exponentiation (base exp) (- (exponent exp) 1))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

;;
;; Add support for exponentiation.
;;
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base n) (cadr n))

(define (exponent n) (caddr n))

(define (make-exponentiation b n)
  (cond ((= n 0) 1)
	((= n 1) b)
	(else
	 (list '** b n))))