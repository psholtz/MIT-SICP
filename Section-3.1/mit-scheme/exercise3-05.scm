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

	;; Inside the circle only if distance-to-center is less than radius
	(<= distance-from-center r)))))

(define predicate-for-unit-circle
  (predicate-for-circle 0 0 1))

;;
;; Import geometry package from Section 2.1:
;;
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (distance-points p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
	   (square (- (y-point p1) (y-point p2))))))
(define (length-segment s)
  (distance-points (start-segment s) (end-segment s)))
  
(define (make-rect upper-left-x upper-left-y lower-right-x lower-right-y)
  (cons (make-point upper-left-x upper-left-y)
	(make-point lower-right-x lower-right-y)))
(define (upper-left-rect rect)
  (car rect))
(define (lower-right-rect rect)
  (cdr rect))
(define (width-rect rect)
  (let ((p1 (upper-left-rect rect))
	(p2 (lower-right-rect rect)))
    (let ((p3 (make-point
	       (x-point p2)
	       (y-point p1))))
      (length-segment (make-segment p1 p3)))))
(define (height-rect rect)
  (let ((p1 (upper-left-rect rect))
	(p2 (lower-right-rect rect)))
    (let ((p3 (make-point
	       (x-point p1)
	       (y-point p2))))
      (length-segment (make-segment p1 p3)))))
(define (perimeter-rect rect)
  (+ (* 2 (width-rect rect)) (* 2 (height-rect rect))))
(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

;;
;; Define predicate for a circle (at origin) with a given radius:
;;
;;(define (circle-with-radius radius)
;;  (lambda (x y)
;;    (let ((distance (sqrt (+ (* x x) (* y y)))))
;;      (<= distance radius))))

;;
;; Define predicate for the unit circle:
;;
;;(define unit-circle (circle-with-radius 1))

;;
;; Use "interval arthimetic" of Chapter 2 for representating intervals:
;;
;;(define (make-interval a b)
;;  (cond ((< a b) (cons a b))
;;	(else
;;	 (display "error constructing interval!"))))
;;(define (lower-bound x) (car x))
;;(define (upper-bound x) (cdr x))
;;(define (range x) (- (upper-bound x) (lower-bound x)))

;;
;; Define the monte-carlo integration:
;;
(define (estimate-integral predicate x-bounds y-bounds trials)
  (let ((x (random-in-range (lower-bound x-bounds) (upper-bound x-bounds)))
	(y (random-in-range (lower-bound y-bounds) (upper-bound y-bounds))))
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
	     (/ trials-passed trials))
	    ((predicate x y)
	     (iter (- trials-remaining 1) (+ trials-passed 1)))
	    (else
	     (iter (- trials-remaining 1) trials-passed))))
    (let ((fraction (iter trials 0))
	  (area (* (range x-bounds) (range y-bounds))))
      (* fraction area))))

  


;;;;
;; do you need this?
(define (random-update x)
  (random (expt 2 31)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 number)
  (define (trial)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (if (p x y)
	  1
	  0)))
  (define (iter n t)
    (if (= n number)
	t
	(iter (+ n 1) (+ t (trial)))))
  
  (let ((fraction (/ (* (iter 0 0) 1.0) (* number 1.0)))
	(area (* (- x2 x1) (- y2 y1))))
    (* area fraction)))


  ;;(let ((fraction (/ (iter 0 0) number))
;;	(area (- x2 x1) (- y2 y1)))
  ;;  (* fraction area)))

(define x1 0)
(define x2 10)
(define y1 0)
(define y2 10)
(define number 100)

(define unit-predicate (lambda (x y) #t))
(define unit-circle-predicate 
  (lambda (x y)
    (let ((radius (sqrt (+ (* x x) (* y y)))))
      (< radius 1))))

