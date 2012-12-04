;;
;; Exercise 2.46
;;
;; A two-dimensional vector v running from the origin to a point can be represented as a pair consisting 
;; of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor
;; "make-vect" and corresponding selectors "xcor-vect" and "ycor-vect". In terms of your selectors and constructor,
;; implement procedures "add-vect", "sub-vect" and "scale-vect" that perform the operations vector addition, 
;; vector subtraction and multiplying a vector by a scalar.
;;

;;
;; Constructors and Selectors:
;;
(define (make-vect x y)
  (list x y))

(define (xcor-vect p)
  (car p))

(define (ycor-vect p)
  (cadr p))

;;
;; Mathematical operations:
;;
(define (add-vect a b)
  (make-vect 
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect c a)
  (make-vect
   (* c (xcor-vect a))
   (* c (ycor-vect a))))

;;
;; Run some unit tests:
;;
(define v (make-vect 1 2))
;; ==> (1 2)

(xcor-vect v)
;; ==> 1

(ycor-vect v)
;; ==> 2

(define u (make-vect 3 4))
;; ==> (3 4)

;;
;; Test out the mathematical operations:
;;
(add-vect u v)
;; ==> (4 6)
(xcor-vect (add-vect u v))
;; ==> 4
(ycor-vect (add-vect u v))
;; ==> 6

(sub-vect u v)
;; ==> (2 2)

(scale-vect 3 u)
;; ==> (9 12)