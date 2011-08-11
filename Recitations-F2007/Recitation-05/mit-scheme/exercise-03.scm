;;
;; Exercise 3
;;
;; Create a data abstraction for points in a place. It should have a constructor, 
;; "(make-point x y)", which returns a point, and two selectors "(point-x p)" 
;; and "(point-y p)" which return the x- and y-coordinates.
;;

;;
;; Define constructor:
;;
(define (make-point x y)
  (cons x y))

;;
;; Define selectors:
;;
(define (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

