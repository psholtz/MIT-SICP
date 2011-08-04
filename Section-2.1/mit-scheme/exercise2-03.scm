;;
;; Exercise 2.3
;;

;;
;; Let's bring in the procedures from Exercise 2.2 that we'll be using here.
;;
;; First the point procedures:
;;
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;;
;; and also the segment procedures:
;;
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;;
;; If we want to calculate the perimeter and area of a rectangle, we'll 
;; have to be able to calculate the length of a line segment, and also
;; the distance between two points:
;;
(define (distance-points p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2))))))
(define (length-segment s)
  (distance-points (start-segment s) (end-segment s)))

;;
;; Let's run some unit tests, to make sure our distance and length procedures 
;; work as advertised:
;;


;;
;; For our first representation of a rectangle, let's define it using 
;; two points: (a) the upper-left hand corner, and (b) the lower right-hand
;; corner. From this we should be able to extract the width and height of the 
;; rectangle, and hence its area and perimeter.
;;
(define (make-rect p1 p2)
  (cons p1 p2))
 
;;
;; Next, let's define selectors that give us the 4-points of the rectangle.
;; These procedures will be "implementation specific" in terms of the
;; representation we have chosen, but all other procedures we define above
;; these should be sufficiently abstract to work with any specific way 
;; of implementing construction of rectangles.
;;
;; We avoid defining any of these procedures in terms of the others (even though
;; we could), to avoid the possibility of (infinite) cycles. 
;;
(define (upper-left-corner-rect r)
  (car r))

(define (upper-right-corner-rect r)
  (make-point (x-point (cdr r)) (y-point (car r))))

(define (lower-left-corner-rect r)
  (make-point (x-point (car r)) (y-point (cdr r))))

(define (lower-right-corner-rect r)
  (cdr r))

;;
;; Let's defien the "width segment" to the top horizontal segment of the rectangle
;;
(define (width-segment-rect r)
  (make-segment (upper-left-corner-rect r) (upper-right-corner-rect r)))
(define (height-segment-rect r)
  (make-segment (upper-left-corner-rect r) (lower-left-corner-rect r)))

;;
;; Now let's define the "width" and "height" of the rectangle. 
;;
;; These should be real numbers, not line segments represented by points.
;;
(define (width-rect r)
  (length (width-segment r)))

(define (height-rect r)
  (length (height-segment r)))

;;
;; Now define the "Area" and "perimeter" procedures;
;;
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define (perimeter-rect r)
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))

;;
;; PUT UNIT TESTS HERE
;;


;;
;; Now define a second representation
;; (these are "segment" objects)
;;
(define (make-rect w h)
  (cons w h))


  


  
