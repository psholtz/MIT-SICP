;;
;; Exercise 2.2
;;

;;
;; Define the "segment" procedures:
;;
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;
;; Define the "point" procedures:
;;
(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

;;
;; Define code for finding the midpoints of segments:
;;
(define (midpoint-segment s)
  (let ((p1 (start-segment s))
	(p2 (end-segment s)))
    (make-point
     (average (x-point p1) (x-point p2))
     (average (y-point p1) (y-point p2)))))

(define (average x y)
  (/ (+ x y) 2))

;;
;; Define the display procedure:
;;
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;
;; Run some unit tests:
;;
;; Let's start by taking opposite corners of a unit square.
;; These should "average out" to a midpoint at the origin.
;;
(define p1 (make-point 1 1))
(define p2 (make-point -1 -1))
(define s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0,0)

(define p1 (make-point 1 -1))
(define p2 (make-point -1 1))
(define s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0,0)