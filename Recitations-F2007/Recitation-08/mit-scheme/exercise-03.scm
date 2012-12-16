;;
;; Working definitions
;;
(define (variable? exp) 
  (symbol? exp))
(define (make-variable var)
  var)
(define (variable-name exp)
  exp)

(define (or? exp)
  (and (pair? exp) (eq? (car exp) 'or)))
(define (make-or exp1 exp2)
  (list 'or exp1 exp2))
(define (or-first exp)
  (cadr exp))
(define (or-second exp)
  (caddr exp))

(define (and? exp)
  (and (pair? exp) (eq? (car exp) 'and)))
(define (make-and exp1 exp2)
  (list 'and exp1 exp2))
(define (and-first exp)
  (cadr exp))
(define (and-second exp)
  (caddr exp))

;;
;; Previous exercises
;;
(define (not? exp)
  (and (pair? exp) (eq? (car exp) 'not)))
(define (make-not exp)
  (list 'not exp))
(define (not-first exp)
  (cadr exp))

;;
;; Exercise 3
;;
;; Given a boolean expression and a set of variable assignments, evaluate the expression to 
;; decide whether the result if #t or #f. Assume that you have a procedure (variable-value
;; name environment), which takes a variable and and a list of values and returns the value
;; assigned to the variable, if a binding for it exists, or throws an error if no binding is 
;; found.
;;

;;
;; As with some of the other examples in Chapter 2, it's easier to get a handle on these 
;; exercies if we peek ahead to Chapter 3 and use the "table" structure defined there, to 
;; use for our symbol bindings and execution environment.
;;
;; We'll import the relevant table definitions:
;;
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else
	 (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

;;
;; Let's define a symbol table to use as our environment:
;;
(define env (make-table))

;;
;; And let's put some variable bindings in there:
;;
(insert! 'x 1 env)
(insert! 'y 2 env)
(insert! 'z 3 env)
(insert! 'key 'value env)

env
;; ==> (*table* (key . value) (z . 3) (y . 2) (x . 1))

;;
;; We can now define the "variable-value" procedure:
;;
(define (variable-value name environment)
  (define (variable-value-iter working)
    (if (null? working)
	(error "VARIABLE-VALUE: no binding for variable: " name)
	(let ((value (car working)))
	  (if (equal? name (car value))
	      (cdr value)
	      (variable-value-iter (cdr working))))))
  (variable-value-iter (cdr environment)))

;; 
;; Unit tests:
;;
(variable-value 'x env)
;; ==> 1
(variable-value 'y env)
;; ==> 2
(variable-value 'z env)
;; ==> 3
(variable-value 'key env)
;; ==> value
(variable-value 'value env)
;; ==> #[error]

;;
;; To get the examples of this exercise to work, let's bind three variable symbols 
;; "a", "b" and "c" to true, true and false:
;;
(insert! 'a #t env)
(insert! 'b #t env)
(insert! 'c #f env)

(variable-value 'a env)
;; ==> #t
(variable-value 'b env)
;; ==> #t
(variable-value 'c env)
;; ==> #f

;;
;; Now to answer the question:
;;
(define (eval exp env)
  ;; return the boolean value of the argument symbol
  (define (boolean-value sym)
    (if (variable? sym)
	(variable-value sym env)
	(not (or (null? sym) (eq? sym #f)))))

  ;; evaluate the boolean expression
  (cond ((or? exp)
	 (let ((first (or-first exp))
	       (second (or-second exp)))
	   (or (boolean-value first) (boolean-value second))))

	((and? exp)
	 (let ((first (and-first exp))
	       (second (and-second exp)))
	   (and (boolean-value first) (boolean-value second))))

	((not? exp)
	 (let ((first (not-first exp)))
	   (not (boolean-value first))))

	(else
	 (error "EVAL - expression is not a boolean expression: " exp))))

;;
;; Let's test it using the boolean expression constructors we have defined:
;;
(eval (make-and 'a 'b) env)
;; ==> #t
(eval (make-or 'a 'b) env)
;; ==> #t
(eval (make-not 'a) env)
;; ==> #f
(eval (make-not 'b) env)
;; ==> #f

(eval (make-and 'a 'c) env)
;; ==> #f
(eval (make-or 'a 'c) env)
;; ==> #t
(eval (make-and 'a 'd) env)
;; ==> #[error]

(eval (make-and 'a #t) env)
;; ==> #t
(eval (make-and 'a 1) env)
;; ==> #t
(eval (make-and 'a 0) env)
;; ==> #t 
;; Note that "0" is not false in Scheme!
(if 0 1 2)
;; ==> 1 
;; i.e., "0" evaluates to "true"

(eval (make-and 'a #f) env)
;; ==> #f
(eval (make-and 'a '()) env)
;; ==> #f