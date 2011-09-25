;;
;; Exercise 2.2
;;
;; Consider the problem of representing line segments in a place. Each segment is represented
;; as a pair of points: a starting point and an ending point. Define a constructor "make-segment"
;; and selectors "start-segment" and "end-segment" that define a representation of segments
;; in terms of points. Furthermore, a point can be represented as a pair of numbers: the 
;; x-coordinate and the y-coordinate. Accordingly, specify a constructor "make-point" and 
;; selectors "x-point" and "y-point" that define this representation. Finally, using your 
;; selectors and constructors, define a procedure "midpoint-segment" that takes a line
;; segment as argument and returns its midpoint (the point whose coordinates are the average
;; of the coordinates of the endpoints). To try your procedures, you'll need a way to print points:
;;
;; (define (print-point p)
;;  (newline)
;;  (display "(")
;;  (display (x-point p))
;;  (display ",")
;;  (display (y-point p))
;;  (display ")"))
;;

;;
;; Define the "segment" procedures:
;;
(defun make-segment (start-point end-point)
  (cons start-point end-point))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

;;
;; Define the "point" procedures:
;;
(defun make-point (x y)
  (cons x y))

(defun x-point (p) (car p))

(defun y-point (p) (cdr p))

;;
;; Define code for finding the midpoints of segments:
;;
(defun midpoint-segment (s)
  (let ((p1 (start-segment s))
	(p2 (end-segment s)))
    (make-point
     (average (x-point p1) (x-point p2))
     (average (y-point p1) (y-point p2)))))

(defun average (x y) (/ (+ x y) 2.0))

;;
;; Define the display procedure:
;;
(defun print-point (p)
  (newline)
  (princ "(")
  (princ (x-point p))
  (princ ",")
  (princ (y-point p))
  (princ ")")
  (newline))

;;
;; Run some unit tests:
;;
;; Let's start by taking opposite corners of a unit square.
;; These should "average out" to a midpoint at the origin.
;;
(setq p1 (make-point 1 1))
(setq p2 (make-point -1 -1))
(setq s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0.0, 0.0)

(setq p1 (make-point 1 -1))
(setq p2 (make-point -1 1))
(setq s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0.0, 0.0)

;;
;; Looks good so far. Let's run some more, arbitrary unit tests:
;;
(setq p1 (make-point 1 5))
(setq p2 (make-point 3 -7))
(setq s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (2.0, -1.0)

(setq p1 (make-point 10 20))
(setq p2 (make-point -2 -6))
(setq s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (4.0, 7.0)