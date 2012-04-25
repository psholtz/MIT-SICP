

;;
;; Define the "segment" procedures:
;;
(defn make-segment
  {:doc "Construct a line segment, given the start point and ending point."}
  [start-point end-point]
  ;;
  ;; Clojure has a different notion of "cons" than other traditional Lisps, hence close with "list"..
  ;;
  (cons start-point (list end-point)))

(defn start-segment
  {:doc "Accessor to get the start point of a line segment."}
  [s]
  (first s))

(defn end-segment
  {:doc "Accessor to get the ending point of a line segment."}
  [s]
  (second s))

;;
;; Define the "point" procedures:
;;
(defn make-point
  {:doc "Construct a point, given the x and y coordinates."}
  [x y]
  (cons x (list y)))

(defn x-point {:doc "Accessor to get x-coordinate of a point."} [p] (first p))
(defn y-point {:doc "Accessor to get y-coordinate of a point."} [p] (second p))

;;
;; Define code for finding the midpoints of segments:
;;
(defn average {:doc "Return average of x and y."} [x y] (/ (+ x y) 2.0))

(defn midpoint-segment
  {:doc "Return the point that is at the midpoint of the argument line segment."}
  [s]
  (let [p1 (start-segment s)
        p2 (end-segment s)]
    (make-point
     (average (x-point p1) (x-point p2))
     (average (y-point p1) (y-point p2)))))

;;
;; Define the display procedure:
;;
(defn print-point [p]
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")")
  (println ""))