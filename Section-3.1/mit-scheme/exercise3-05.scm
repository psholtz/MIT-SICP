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

    ;; Define M.C. test
    (define (estimate-integral-test)
      (let ((x (random-in-range x1f x2f))
	    (y (random-in-range y1f y2f)))
	(predicate x y)))

    ;; Integral is given by multiplying area of rectangle by M.C. results
    (let ((area-rect (* (- x2 x1) (- y2 y1))))
      (* area-rect (monte-carlo trials estimate-integral-test)))))

;;
;; Run unit tests:
;;

;; [working]
