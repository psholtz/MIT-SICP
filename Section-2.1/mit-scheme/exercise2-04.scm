;;
;; Exercise 2.4
;;
;; Here is an alternative procedural representation of pairs. For this representation, 
;; verify that (car (cons x y)) yields x for any objects x and y.
;;
;; (define (cons x y)
;;  (lambda (m) (m x y)))
;;
;; (define (car z)
;;  (z (lambda (p q) p)))
;;
;; What is the corresponding definition of cdr? 
;;

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;
;; We can prove this result by using the evaluation model of evaluation:
;;
;; We begin with the expression for (car (cons x y)):
;;
(car (cons x y))

;; 
;; Replacing (car z) with its definition in terms of the lambda function:
;;
((cons x y) (lambda (p q) p))

;;
;; Expanding (cons x y) in terms of its definition in terms of lambda:
;;
((lambda (m) (m x y)) (lambda (p q) p))

;;
;; This expression looks formidable, but it yields the answer we are seeking.
;; Specifically, the left-most subexpression (the operator) is simply an 
;; (anonymous) procedure which takes one argument: that argument itself being 
;; a procedure which takes two arguments.  
;;
;; The syntax of this expression is therefore consistent. 
;; 
;; Although it deviates slightly from the strict model of substitution evaluation, 
;; it might make things clearer if we made the following definition:
;;
(define f (lambda (p q) p))

;;
;; The above expression then can be written as:
;;
((lambda (m) (m x y)) f)

;; 
;; Which reduces to:
;;
(f x y)

x

;;
;; Which is the result we were seeking to prove.
;;

;;
;; The corresponding definition of cdr would be:
;;
(define (cdr z)
  (z (lambda (p q) q)))