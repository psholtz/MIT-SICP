;;
;; Exercise 2.8
;; 
;; Using reasoning similar to Alyssa's, describe how the difference of two intervals
;; may be computed. Define a corresponding subtraction procedure, called "sub-interval".
;;

;;
;; First, for the sake of completeness, let's import our constructor procedures from the 
;; previous exercise, as well as the other interval arithmetic operations defined in the text.
;;

;;
;; Constructors and Selectors
;;
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;;
;; Arithmetical Procedures:
;;
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;
;; Now let us construct the substraction procedure. 
;;
;; First we will define a "negative" procedure, that creates an arithmetical 
;; inverse for a given interval. Then we will add this "negative" to the 
;; first element of the combination, to arrive at the difference between the two elements.
;;
(define (negative interval)
  (make-interval (* -1 (upper interval))
		 (* -1 (lower interval))))

;;
;; Do a few use cases, to verify that we are getting the negative:
;;
(negative (make-interval 3 5))

(negative (make-interval 0 3))

(negative (make-interval -5 -3))

(negative (make-interval -3 0))
 
;;
;; Now define the subtraction procedure:
;;
(define (sub-interval x y)
  (let ((z (negative y)))
    (add-interval x z)))