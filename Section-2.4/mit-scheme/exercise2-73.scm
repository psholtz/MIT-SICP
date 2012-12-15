;;
;; Exercise 2.73
;;
;; Section 2.3.2 described a program that performs symbolic differentiation:
;;
;;  (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         (else (error "Unknown expression type -- DERIV" exp))))
;;
;; We can regard this program as performing a dispatch on the type of the expression to be differentiated.
;; In this situation the ``type tag'' of the datum is the algebraic operator symbol (such as +) and the operation
;;  being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative 
;; procedure as
;;
;;  (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get 'deriv (operator exp)) (operands exp) var))))
;;
;; (define (operator exp) (car exp))
;; (define (operands exp) (cdr exp))
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
;; Install the differentiator:
;;
(install-symbolic-differentiator)
;; ==> 'done

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
;; (a)  Explain what was done above. Why can't we assimilate the predicates number? and same-variable? 
;;      into the data-directed dispatch?
;;

;;
;; In the earlier definition of "deriv", the differentiation rules that were supported by "deriv" were 
;; hard-coded into the procedure. So if we wanted to add new differentiation rules for new mathematical
;; operations (i.e., adding exponentiation), or if we wanted to change the way that a differentiation 
;; rule worked (i.e., switch from binary operations to n-ary operations), we had to rewrite the entire
;; "deriv" procedure. This is opens the possibility of introducing errors, and in a "real-life" commercial
;; settings introduces the problem of how to update code that has already been distributed out into the 
;; marketplace.
;;
;; With the new data-driven model, we never need to redefine "deriv". The definition for this procedure
;; can remain as it is, and support for new procedures can be added dynamically. Similarly, the way support
;; for existing procedures is implemented can be changed dynamically. All that is required is to update 
;; the database/table where the procedures are defined. This can be done without redefining or changing
;; the "deriv" procedure.
;;

;;
;; To answer the question of why the predicates "number?" and "same-variable?" cannot be assimilated
;; into the data-directed dispatch routine, let's see what would happen were we to try to do so:
;;
;; We would (presumably) redefine "dervi" something like:
;;
;; (define (deriv exp var)
;;  (cond ((get 'deriv (operator exp)) (operands exp) var)))
;;
;; and immediately we see that we would be reduced to evaluating expressions like:
;;
;;  (operator 1)
;;  (operands 1)
;;
;; and/or
;;
;;  (operator x)
;;  (operands x)
;;
;; which, given our definition of "operator" and "operands", would signal errors since these
;; procedures are designed to extract the operator and operands from a particular expression 
;; (which expression is expected to be in the form of a list structure), and then dispatch on 
;; the type of the submitted operation.
;;
;; Here, since there is no "operation" and there is consequently no "type", there is nothing 
;; to dispatch on. Consequently, these two use cases must be handled using special predicates 
;; like "number?" and "same-variable?".
;;

;;
;; (b) Write the procedures for derivatives of sums and products, and the auxiliary code required to install 
;;     them in the table used by the program above.
;;

;;
;; Already did that above.
;;

;;
;; (c) Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and 
;;     install it in this data-directed system.
;;

;;
;; Already did that above.
;;

;;
;; (d) In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. 
;; Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like
;;
;;  ((get (operator exp) 'deriv) (operands exp) var)
;;
;; What corresponding changes to the derivative system are required?
;;

;;
;; We would need to change our "installation" procedure and the "deriv" procedure itself:
;;

(define (install-symbolic-differentiator)
  (put '+ 'deriv (lambda (exp var)
		   (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var))))
  (put '- 'deriv (lambda (exp var)
		   (make-difference (deriv (minuend exp) var)
				    (deriv (subtrahend exp) var))))
  (put '* 'deriv (lambda (exp var)
		   (make-sum
		    (make-product (multiplier exp)
				  (deriv (multiplicand exp) var))
		    (make-product (deriv (multiplier exp) var)
				  (multiplicand exp)))))
  (put '** 'deriv (lambda (exp var)
		    (make-product (exponent exp)
				  (make-exponentiation 
				   (base exp) 
				   (make-difference (exponent exp) 1)))))
  'done)

(install-symbolic-differentiator)
;; ==> 'done

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else
	 ((get 'deriv (operator exp)) (operands exp) var))))

;;
;; Other than this, no other changes are required.
;;

;;
;; Running the unit tests above generates the same results
;; using the new data model/architecture.
;;