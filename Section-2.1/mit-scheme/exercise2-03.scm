;;
;; Exercise 2.3
;;

;;
;; Let's bring in the procedures from Exercise 2.2 that we'll be using here.
;;
;; The point procedures:
;;
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;;
;; And the segment procedures:
;;
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;;
;; Give two points of the rectangle:
;; 
;; (a) upper-left corner; and 
;; (b) lower-right corner;
;; 
(define (make-rect p1 p2)
  (cons p1 p2))
(define (upper-left-rect r)
  (car r))
(define (upper-right-rect r)
  (make-point (x-point (lower-right-rect r)) (y-point (upper-left-rect r))))
(define (lower-left-rect r)
  (make-point (x-point (upper-left-rect r)) (y-point (lower-right-rect r))))
(define (lower-right-rect r)
  (cdr r))

;;
;; define a distance metrics for the space we are workign w/
;;
(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2))))))

(define (length segment)
  (distance (start-segment segment) (end-segment segment)))
;;
;; 
(define (width-segment-rect r)
  (make-segment (upper-left-rect r) (upper-right-rect r)))
(define (height-segment-rect r)
  (make-segment (upper-left-rect r) (lower-left-rect r)))

;;
;; Now define the generalized selectors, and define the "perimeter" and 
;; "area" procedures in terms of these generalized selectors.
;;
;; These represent the "abstraction" barriers.
;;
(define (width-rect r)
 

  (make-segment (upper-left-rect r) (upper-right-rect r)))
(define (height-rect r)
  (make-segment (upper-left-rect r) (lower-left-rect r)))

;;
;; Now define the "Area" and "perimeter" procedures;
;;
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define (perimeter-rect r)
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))


;;
;; Now define a second representation
;; (these are "segment" objects)
;;
(define (make-rect w h)
  (cons w h))
(Define 


  


  
