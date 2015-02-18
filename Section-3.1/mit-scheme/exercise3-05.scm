;; [working]


;;
;; Define the "random-in-range" procedure given in text:
;;
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;
;; Define supporting procedures:
;;
(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2.0))

;;
;; Define circle predicates. 
;;
;; The following procedure returns a predicate for testing whether the 
;; point (x,y) is within the circle of radius r centered at point (a,b):
;;
(define (predicate-for-circle a b r)
  (lambda (x y)
    (let ((dx (- a x))
	  (dy (- b y)))
      (let ((distance-from-center (sqrt (+ (square dx) (square dy)))))

	;; Inside the circle only if distance-from-center is less than radius
	(<= distance-from-center r)))))

(define predicate-for-unit-circle
  (predicate-for-circle 0 0 1))

;;
;; Monte Carlo procedure as defined in text.
;;
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0.0))

;; 
;; Estimation procedure.
;;
;; Invoke procedure where (x1, x2) and (y1, y2) represent bounds for a range, 
;; as specified in the problem statement, so that x1 < x2 and y1 < y2, and so
;; (x1, y2) is the upper-left corner of the rectangle, and (x2, y1) is the 
;; lower-right corner of the rectangle.
;;
;; Moreover, this procedure only makes sense if the rectangle is a square.
;;
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  ;; Make sure we are working with floats, otherwise random is pointless
  (let ((x1f (* 1.0 x1))
	(x2f (* 1.0 x2))
	(y1f (* 1.0 y1))
	(y2f (* 1.0 y2)))

    ;; Define M.C. test by selecting random point (x,y)
    (define (estimate-integral-test)
      (let ((x (random-in-range x1f x2f))
	    (y (random-in-range y1f y2f)))
	(predicate x y)))

    ;; Integral is given by multiplying area of rectangle by M.C. results
    (let ((area-rect (* (- x2 x1) (- y2 y1))))
      (* area-rect (monte-carlo trials estimate-integral-test)))))

;; 
;; The area of the unit circle is A(C) = pi * 1^1 = pi. 
;;
;; Hence by invoking the estimate-integral procedure with the predicate
;; for the unit circle, we should expect to obtain a result that approximates
;; the value of pi:
;;

(estimate-integral predicate-for-unit-circle -1 1 -1 1 10)
;; ==> 3.2

;;
;; The bounding rectangle for the unit circle is given by (-1,1) in the 
;; x-dimension and (-1,1) in the y-dimension, hence the choices for x1, x2 
;; and y1, y2 above. We could choose other dimensions for the bounding 
;; rectangle as well, so long as the unit circle is completely contained 
;; within the bounding rectangle.
;;

;;
;; Monte Carlo is a powerful integration technique when other methods prove
;; intractable, but convergence can be quite slow. It is frequently necessary 
;; to simulate a large number of trials before the approximation begins 
;; converging to the correct answer:
;;

(estimate-integral predicate-for-unit-circle -1 1 -1 1 100)
;; ==> 3.12
(estimate-integral predicate-for-unit-circle -1 1 -1 1 1000)
;; ==> 3.172
(estimate-integral predicate-for-unit-circle -1 1 -1 1 10000)
;; ==> 3.1628