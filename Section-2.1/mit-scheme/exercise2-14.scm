;;
;; Exercise 2.14
;;


;;
;; Constructors and Selectors:
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
;; Let's add the "make-center-width" and "make-center-percent" constructors:
;;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))
(define (percent i)
  (/ (width i) (center i)))

;;
;; Next define the two "parallel" procedures, and see whether they give inconsistent answers:
;;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
;;
;; Let's define two intervals, and see what the results are:
;;
(define x (make-center-percent 1 0.1))
;; ==> (0.9 . 1.1)
(define y (make-center-percent 2 0.1))
;; ==> (1.8 . 2.2)

(par1 x y)
;; ==> (0.4909090909 0.896296296296)
(par2 x y)
;; ==> (0.6 0.73333333)

;;
;; Clearly these two numbers are off by quite a bit. 
;;
;; Let's get a better idea of how far apart they are:
;;
(center (par1 x y))
;; ==> 0.6936
(center (par2 x y))
;; ==> 0.6666

;;
;; The centers are "close", but clearly do not precisely coincide.
;;

(width (par1 x y))
;; ==> 0.20269
(width (par2 x y))
;; ==> 0.0666

;;
;; The widths are very "different", again clearly indicating that these are not "similar" intervals.
;;

(percent (par1 x y))
;; ==> 0.292233
(percent (par2 x y))
;; ==> 0.1

;;
;; The percentage error using the first procedure is nearly three times the error produced using
;; the second procedure.
;;

;;
;; As an additional check, let's see that the two "parallel" operations are 
;; at least symmetric, which they should be:
;;
(equal? (par1 x y) (par1 y x))
;; ==> #t

(equal? (par2 x y) (par2 y x))
;; ==> #t

;;
;; Let's see how the two methods fare when using values in a more "practical" context.
;; Suppose we have one resistor, rated 40 kOhm with an error of 0.5%, and a second 
;; resistor rated at 65 kOhm with an error tolerance of 1.25%.
;;
(define x (make-center-percent 40000 0.005))
(define y (make-center-percent 65000 0.0125))

(par1 x y)
;; ==> (24097.75 . 25442.12)
(par2 x y)
;; ==> (24567.02 . 24956.14)

;;
;; Clearly the intervals are "off", but not by so large a margin as before.
;;
;; Let's compare the center points and percentage width:
;;
(center (par1 x y))
;; ==> 24769.9
(center (par2 x y))
;; ==> 24761.6

;;
;; Indeed, the center points are quite close!
;;
(percent (par1 x y))
;; ==> 0.0271
(percent (par2 x y))
;; ==> 7.86e-3

;;
;; The percentage width is about 3 times wider using the "par1" procedure than 
;; when using the "par2" procedure, but both percentage widths are quite "tight".
;;

;;
;; The problem statement indicates that "you will get the most insight by using
;; intervals whose width is a small percentage of the center value." I'm not sure
;; that I'm able to reproduce this finding. When the interval width is a small 
;; percentage of the center value, as in the above example, the two procedures seem 
;; to agree quite closely. The large "errors" seem to arise - on the contrary - only
;; when the percentage width is quite large compared to the center value.
;;

;;
;; As a final example, let's use an example where the percentage width is
;; egregiously large compared to the center value:
;;
(define x (make-center-percent 10 0.3))
;; ==> (7 . 13)
(define y (make-center-percent 10 0.4))
;; ==> (6 . 14)

;;
;; Both intervals (clearly) have the same center:
;;
(center x)
;; ==> 10. 
(center y)
;; ==> 10.

;;
;; However, running these intervals through the two parallel resistor calculations
;; gives wildly divergent answers:
;;
(par1 x y)
;; ==> (1.5555 . 14.)
(par2 x y)
;; ==> (3.23077 . 6.74074)

;;
;; The "center" of par1 does not even lie within the bounds of par2!
;;
(center (par1 x y))
;; ==> 7.777777
(center (par2 x y))
;; ==> 4.98575
(> (center (par1 x y)) (upper-bound (par2 x y)))
;; ==> #t

;;
;; So it's true, the "same" algebraic expression can give rise to "different" answers
;; using the interval arithemetic, however I am not able to reproduce the "hint" given 
;; in the text, wherein the most widely divergent answers are obtained by using "small" 
;; percentage widths relative to the center value. Rather, I seem to obtain the most 
;; widely divergent answers when using percentage widths that are very large in comparison
;; to the center value.
;;