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
(defn make-point [x y]
  (list x y))

;;
;; Define selectors:
;;
(defn point-x [p]
  (first p))

(defn point-y [p]
  (first (rest p)))