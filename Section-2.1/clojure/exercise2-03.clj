
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
(defn square [x] (* x x))
(defn distance-points [p1 p2]
  (Math/sqrt (+ (square (- (x-point p1) (x-point p2)))
                (square (- (y-point p1) (y-point p2))))))
(defn length-segment [s]
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
