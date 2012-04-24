

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