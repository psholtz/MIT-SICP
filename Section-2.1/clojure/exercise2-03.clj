
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
(defn make-rectangle [upper-left-x upper-left-y lower-right-x lower-right-y]
  (cond (and (> lower-right-x upper-left-x) (> upper-left-y lower-right-y))
         (cons (make-point upper-left-x upper-left-y)
               (list (make-point lower-right-x lower-right-y)))
         :else
         (println "Error: rectangle dimensions are invalid!")))
        