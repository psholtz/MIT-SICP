;;
;; Exercise 2.48
;;
;; A directed line segment in a plane can be represented as a pair of vectors -- the vector running from the 
;; origin to the start-point of the segment, and the vector running from the origin to the end-point of the 
;; segment. Use your vector representation from Exercise 2.46 to define a representation for segments with 
;; a constructor "make-segment" and selectors "start-segment" and "end-segment".
;;

;;
;; First import the necessary code from Exercise 2.46:
;;
(define (make-vect x y)
  (list x y))
(define (xcor-vect p)
  (car p))
(define (ycor-vect p)
  (cadr p))

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
;; Now define the constructors and selectors:
;;
(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

;;
;; Run some unit tests:
;;
(define p1 (make-vect 1 2))
;; ==> (1 2)
(define p2 (make-vect 3 5))
;; ==> (3 5)

(define s (make-segment p1 p2))
;; ==> ((1 2) (3 5))
(start-segment s)
;; ==> (1 2)
(end-segment s)
;; ==> (3 5)