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
(defun make-interval (a b) (cons a b))
(defun lower-bound (x) (car x))
(defun upper-bound (x) (cdr x))

;;
;; Arithmetical Procedures:
;;
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
     
(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))


;;
;; Now let us construct the substraction procedure. 
;;
;; First we will define a "neg-internal" procedure, that creates an arithmetical 
;; inverse for a given interval. Then we will add this "negative" to the 
;; first element of the combination, to arrive at the difference between the two elements.
;;
(defun neg-interval (interval)
  (make-interval (* -1 (upper-bound interval))
		 (* -1 (lower-bound interval))))

;;
;; Do a few use cases, to verify that we are getting the negative:
;;
(neg-interval (make-interval 3 5))
;; ==> (-5 . -3)

(neg-interval (make-interval 0 3))
;; ==> (-3 . 0)

(neg-interval (make-interval -5 -3))
;; ==> (3 . 5)

(neg-interval (make-interval -3 0))
;; ==> (0 . 3)

;;
;; Now define the subtraction procedure:
;;
(defun sub-interval (x y)
  (let ((z (neg-interval y)))
    (add-interval x z)))

;;
;; Run some use cases:
;;
(setq i1 (make-interval 3 5))
;; ==> (3 . 5)

(sub-interval i1 i1)
;; ==> (-2 . 2)