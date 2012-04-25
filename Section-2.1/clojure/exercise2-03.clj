
;;
;; First let's import the procedures we designed in Exercise 2.2:
;;
;; Support for Points:
;;
(defn make-point [x y]
  (cons x (list y)))
(defn x-point [p] (first p))
(defn y-point [p] (second p))

;;
;; Support for Line Segments:
;;
(defn make-segment [start-point end-point]
  (cons start-point (list end-point)))
(defn start-segment [s] (first s))
(defn end-segment [s] (second s))

;;
;; Support for Euclidean distances:
;;
(defn square {:doc "Return square of argument."} [x] (* x x))

(defn distance-points
  {:doc "Return Euclidean distance between two points."}
  [p1 p2]
  (Math/sqrt (+ (square (- (x-point p1) (x-point p2)))
                (square (- (y-point p1) (y-point p2))))))

(defn length-segment
  {:doc "Return the length of the argument line segment."}
  [s]
  (distance-points (start-segment s) (end-segment s)))

;;
;; Support for printing Points:
;;
(defn print-point [p]
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")")
  (println ""))

;;
;; Lets start by implementing Model (1).
;;
;; We will construct the rectangle by specifying:
;;
;; (a) upper-left-x
;; (b) upper-left-y
;; (c) lower-right-x
;; (d) lower-right-y
;;
;; We create a data structure to hold these two points (upper-left, and lower-right)
;;
;; We want our constructor to signal an error if the user supplies rectangle dimensions
;; which are invalid:
;;
(defn make-rectangle
  {:doc "Return two point structure representing upper left point, and lower right point of the rectangle."}
  [upper-left-x upper-left-y lower-right-x lower-right-y]
  (cond (and (> lower-right-x upper-left-x) (> upper-left-y lower-right-y))
         (cons (make-point upper-left-x upper-left-y)
               (list (make-point lower-right-x lower-right-y)))
         :else
         (println "Error: rectangle dimensions are invalid!")))

;;
;; Selectors
;;
(defn upper-left {:doc "Return upper left point of rectangle."} [rect] (first rect))

(defn lower-right {:doc "Return lower right point of rectangle."} [rect] (second rect))

(defn width
  {:doc "Return width of the rectangle."}
  [rect]
  (let [p1 (upper-left rect)
        p2 (lower-right rect)]
    (let [p3 (make-point (x-point p2)
                         (y-point p1))]
      (length-segment (make-segment p1 p3)))))

(defn height
  {:doc "Return height of the rectangle."}
  [rect]
  (let [p1 (upper-left rect)
        p2 (lower-right rect)]
    (let [p3 (make-point
              (x-point p1)
              (y-point p2))]
      (length-segment (make-segment p1 p3)))))

;;
;; Now define the "perimeter" and "area" procedures:
;;
(defn perimeter
  {:doc "Return perimeter of the rectangle."}
  [rect]
  (+ (* 2 (width rect)) (* 2 (height rect))))

(defn area
  {:doc "Return area of the rectangle."}
  [rect]
  (* (width rect) (height rect)))

;;
;; Note that we can change the representational model for the rectangles, so long as
;; we do not alter how the "width" and "height" procedures behave.
;;

;;
;; Let's run some use cases.
;;
;; The following cases should all fail, owing to bad dimensions.
;;
(def r1 (make-rectangle 0 0 0 0))
;; ==> Invalid
(def r1 (make-rectangle 0 1 0 0))
;; ==> Invalid
(def r1 (make-rectangle 0 0 1 0))
;; ==> Invalid
(def r1 (make-rectangle 0 0 -1 0))
;; ==> Invalid
(def r1 (make-rectangle 0 0 0 -1))
;; ==> Invalid
(def r1 (make-rectangle 0 0 -1 -1))
;; ==> Invalid
(def r1 (make-rectangle 0 0 1 1))
;; ==> Invalid

;;
;; Now let's start working with "real" rectangles, and run them through our selectors:
;;
(def r1 (make-rectangle 0 1 1 0))
;; ==> #'user/r1
(print-point (upper-left r1))
;; ==> (0,1)
(print-point (lower-right r1))
;; ==> (1,0)
(width r1)
;; ==> 1.0
(height r1)
;; ==> 1.0
(perimeter r1)
;; ==> 4.0
(area r1)
;; ==> 1.0

(def r2 (make-rectangle 0 1 2 0))
;; ==> #'user/r2
(print-point (upper-left r2))
;; ==> (0,1)
(print-point (lower-right r2))
;; ==> (2,0)
(width r2)
;; ==> 2.0
(height r2)
;; ==> 1.0
(perimeter r2)
;; ==> 6.0
(area r2)
;; ==> 2.0

;;
;; Now let's implement representational Model (2).
;;
;; As before, we still require 4 parameters to specify the rectangle, but this time
;; we will use:
;;
;; (a) upper-left-x
;; (b) upper-left-y
;; (c) width
;; (d) height
;;
;; Again, do some error checking to make sure the rectangle is valid.
;;
(defn make-rectangle [upper-left-x upper-left-y width height]
  (cond (and (> width 0) (> height 0))
        (let [p1 (make-point upper-left-x upper-left-y)]
          (let [p2 (make-point
                    (+ (x-point p1) width)
                    (- (y-point p1) height))]
            (cons p1 (list p2))))
        :else
        (println "Error: rectangle dimensions area invalid!")))

;;
;; Note that if we construct the rectangle in this manner, we do not need
;; to alter any of the remaining selectors, nor do we need to change the
;; "perimeter" or "area" procedures.
;;

;;
;; Again, let's check to make sure it rejects bad rectangles:
;;