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

(define (eval-boolean exp env)
  '())