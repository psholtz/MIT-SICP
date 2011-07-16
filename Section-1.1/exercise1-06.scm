;;
;; Execise 1.6
;;

;;
;; Define the new "if" predicate as follows:
;;
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (= 2 3) 0 5)
;; 5

(new-if (= 1 1) 0 5)
;; 0

;;
;; Rewrite the "sqrt" procedure using the new "if" predicate:
;;
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

;;
;; INSERT NEW "IF" PREDICATE HERE
;;
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

;;
;; Attempting to run this code through the Scheme interpreter, which
;; uses applicative-order evaluation, will result in an infinite 
;; recursion. Because new-if is a procedure, rather than a special form, 
;; the interpreter will attempt to evaluate the procedure and its 
;; operands, before applying the procedure to the operands. 
;; In this case, the way in which sqrt-iter is recursively defined, will
;; cause the interpreter to go into an infinite recursion. 
;;
;; Roughly speaking, the evaluation proceeds as follows:
;;
;; (sqrt-iter 1.0 2.0)
;; (new-if (good-enough? 1.0 2.0) 1.0 (sqrt-iter (improve 1.0) 2.0))
;;
;; At this point, the interpreter will attempt to evaluate the operands
;; to new-if, before applying new-if to the operands. (good-enough? 1.0 2.0)
;; evaluates to #f, but sqrt-iter is recursively defined, and includes in 
;; its definition another instance of new-if:
;;
;; (sqrt-iter (improve 1.0) 2.0)
;; (new-if (good-enough? (improve 1.0) 2.0) (improve 1.0) (sqrt-iter (improve (improve 1.0)) 2.0))
;;
;; It's easy to see that this process results in an infinite recursion, hanging
;; the interpreter.
;;

;;
;; This code will hang the interpreter:
;;
(sqrt-iter 1.0 2.0)

