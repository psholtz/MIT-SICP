;;
;; Exercise 2.73
;;
;; [WORKING]
;;

(load "differentiation.scm")

;;
;; (a) Re-define "deriv" as defined in the text:
;;
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variabe? exp) (if (same-variable? exp var) 1 0))
	(else
	 ((get 'deriv (operator exp)) (operands exp) var))))
