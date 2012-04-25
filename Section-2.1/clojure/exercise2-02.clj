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

;;
;; Run some unit tests:
;;
;; Let's start by taking opposite corners of a unit square.
;; These should "average out" to a midpoint at the origin.
;;
(def p1 (make-point 1 1))
(def p2 (make-point -1 -1))
(def s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0.0,0.0)

(def p1 (make-point 1 -1))
(def p2 (make-point -1 1))
(def s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (0.0,0.0)

;;
;; Looks good so far. Let's run some more, arbitrary unit tests:
;;
(def p1 (make-point 1 5))
(def p2 (make-point 3 -7))
(def s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (2.0,-1.0)

(def p1 (make-point 10 20))
(def p2 (make-point -2 -6))
(def s (make-segment p1 p2))
(print-point (midpoint-segment s))
;; ==> (4.0,7.0)