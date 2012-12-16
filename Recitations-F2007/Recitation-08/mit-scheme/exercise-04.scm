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
(define env (make-table))
(define (variable-value name environment)
  (define (variable-value-iter working)
    (if (null? working)
	(error "VARIABLE-VALUE: no binding for variable: " name)
	(let ((value (car working)))
	    (if (equal? name (car value))
		      (cdr value)
		            (variable-value-iter (cdr working))))))
  (variable-value-iter (cdr environment)))
(variable-value 'a env)
(variable-value 'b env)
(variable-value 'c env)
(define (eval-boolean exp env)
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
;; Exercise 4
;; 
;; The evaluator as described so far only allows expressions to be either boolean operators
;; or variable values. Extend the operator so that expressions can include literal boolean as 
;; well, so that evaluating expressions such as (and #t #f) work.
;;

