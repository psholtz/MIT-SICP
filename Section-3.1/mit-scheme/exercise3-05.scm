;; [working]


;;
;; Define the "random-in-range" procedure given in text:
;;
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;
;; Define predicate for a circle (at origin) with a given radius:
;;
(define (circle-with-radius radius)
  (lambda (x y)
    (let ((distance (sqrt (+ (* x x) (* y y)))))
      (<= distance radius))))

;;
;; Define predicate for the unit circle:
;;
(define unit-circle (circle-with-radius 1))

;;
;; Use "interval arthimetic" of Chapter 2 for representating intervals:
;;
(define (make-interval a b)
  (cond ((< a b) (cons a b))
	(else
	 (display "error constructing interval!"))))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (range x) (- (upper-bound x) (lower-bound x)))

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

