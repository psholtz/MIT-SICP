;;
;; Exercise 2.73
;;
;; [WORKING]
;;

;;
;; Load the necessary libraries:
;;
(load "differentiation.scm")

(load "table.scm")

;;
;; Let's also install the procedures we need to support symbolic differentiation:
;;
(define (install-symbolic-differentiator)
  (put 'deriv '+ (lambda (exp var)
		   (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var))))
  (put 'deriv '- (lambda (exp var)
		   (make-difference (deriv (minuend exp) var)
				    (deriv (subtrahend exp) var))))
  (put 'deriv '* (lambda (exp var)
		   (make-sum
		    (make-product (multiplier exp)
				  (deriv (multiplicand exp) var))
		    (make-product (deriv (multiplier exp) var)
				  (multiplicand exp)))))
  (put 'deriv '** (lambda (exp var)
		    (make-product (exponent exp)
				  (make-exponentiation 
				   (base exp) 
				   (make-difference (exponent exp) 1)))))
  'done)

;;
;; Now let's try to define our symbolic differentiator:
;;
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else
	 ((get 'deriv (operator exp)) (operands exp) var))))

;;
;; If we try to run the code at this point, it crashes.
;;
(deriv '(+ x 1) 'x)
;; ==> [CRASH!]

;;
;; The reason is because we are now "abstracting" out the operator symbol
;; using the "operator" procedure, and are passing to our symbolic differentiator
;; only the expression operands themselves.
;;
;; So for instance, before, when invoking "make-sum", we would submit the 
;; expression (+ x 3). Now we are merely submitting the expression (x 3), 
;; with no symbol to indicate the operation being perform (the operation itself
;; is being handled by the data-directed message dispatching).
;;

;;
;; Clearly, then, all we have to do is redefine our expression selector procedures:
;;
(define (addend s) (car s))
(define (augend s) (cadr s))

(define (minuend s) (car s))
(define (subtrahend s) (cadr s))

(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))

(define (base p) (car p))
(define (exponent p) (cadr p))

;;
;; Attempting our derivation again, we obtain:
;;
(deriv '(+ x 3) 'x)
;; ==> 1

;;
;; Let's run through the rest of the unit tests:
;;
(deriv 3 'x)
;; ==> 0
(deriv 'x 'x)
;; ==> 1
(deriv 'x 'y)
;; ==> 0 
(deriv '(+ x y) 'x)
;; ==> 1
(deriv '(+ x y) 'y)
;; ==> 1
(deriv '(+ x y) 'z)
;; ==> 0
(deriv '(+ (* 2 x) y) 'x)
;; ==> 2
(deriv '(+ (* 2 x) y) 'y)
;; ==> 1
(deriv '(+ (* x y) y) 'x)
;; ==> y
(deriv '(+ (* x y) y) 'y)
;; ==> (+ x 1)
(deriv '(- x 1) 'x)
;; ==> 1
(deriv '(- y x) 'x)
;; ==> -1
(deriv '(* x y) 'x)
;; ==> y
(deriv '(** x 3) 'x)
;; ==> (* 3 (** x 2))
(deriv '(** x y) 'x)
;; ==> (* y (** x  (- y 1)))

;;
;; Some more unit tests:
;;
(deriv '(* x y) 'x)
;; ==> y
(deriv '(+ (* x y) (+ x 3)) 'x)
;; ==> (+ y 1)

;;
;; (a) [???] ANSWER THE QUESTION
;;
