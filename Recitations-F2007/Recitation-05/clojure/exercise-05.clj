;;
;; Exercise 5
;;
;; Write a procedure "(intersection seg1 seg2)" that returns a point where two
;; line segments intersect if they do, and returns #f if they do not intersect.
;; Be sure to honor the abstractions defined.
;;

;;
;; It will be useful to first define some helper methods.
;;
;; We would like to know both (a) the slope of the line; and (b) where the line
;; segment would intercept the y-axis if extended in both directions indefinitely
;; (i.e., extend the line segment to a full line).
;;
;; In other words, we would like to rewrite the line segment in terms of "y=mx+b"
;; from standard high school algebra, and use this to solve the problem.
;;

;;
;; A procedure to find the slope of the line segment.
;;
;; Returns the numerical slope of the line when it exists.
;; If the line is vertical and the slope is "infinite", it
;; returns '().
;;
(defn slope [line-segment]
  (let [start (line-segment-start line-segment)
	end (line-segment-end line-segment)]
    (let [dx (- (point-x start) (point-x end))
	  dy (- (point-y start) (point-y end))]
      (if (= dx 0)
	'()
	(* 1.0 (/ dy dx))))))

;;
;; Run some unit tests.
;;
;; Define four points on a square:
;;
(def p1 (make-point 1 1))
(def p2 (make-point 1 -1))
(def p3 (make-point -1 -1))
(def p4 (make-point -1 1))

;;
;; Define the six line segments joining them together:
;;
(def d1 (make-line-segment p1 p2))
(def d2 (make-line-segment p2 p3))
(def d3 (make-line-segment p3 p4))
(def d4 (make-line-segment p4 p1))
(def d5 (make-line-segment p1 p3))
(def d6 (make-line-segment p2 p4))

;;
;; Test the slopes of these line segments:
;;
(slope d1)
;; ==> ()
(slope d2)
;; ==> 0.0
(slope d3)
;; ==> ()
(slope d4)
;; ==> 0.0
(slope d5)
;; ==> 1.0
(slope d6)
;; ==> -1.0

;;
;; So far, so good.
;;
;; Let's make sure that the slope of the line segment is the same,
;; no matter which 'direction' we define the line segment as going in:
;;
(= (slope (make-line-segment p1 p3)) (slope (make-line-segment p3 p1)))
;; ==> true
(= (slope (make-line-segment p2 p4)) (slope (make-line-segment p4 p2)))
;; ==> true
(= (slope (make-line-segment p2 p3)) (slope (make-line-segment p3 p2)))
;; ==> true

;;
;; The test has to be defined slightly differently for "vertical" (i.e.,
;; infinite slope) line segments.
;;
(and (empty? (slope (make-line-segment p1 p2)))
     (empty? (slope (make-line-segment p2 p1))))
;; ==> true

;;
;; Finally, let's test some lines that have differing slopes
;;
(def origin (make-point 0 0))
(def q1 (make-point 1 2))
(def q2 (make-point 1 3))
(def q3 (make-point 2 1))
(def q4 (make-point 3 1))

(slope (make-line-segment origin q1))
;; ==> 2.0
(slope (make-line-segment origin q2))
;; ==> 3.0 
(slope (make-line-segment origin q3))
;; ==> 0.5
(slope (make-line-segment origin q4))
;; ==> 0.33333333333

;;
;; Next we want to determine the y-intercept of the line segment,
;; which would give us the "b" in the y=mx+b formulation.
;;
(defn y-intercept [line-segment]
  (let [p (line-segment-start line-segment)
        m (slope line-segment)]
    (cond (not (= '() m))
          (let [x (point-x p)
                y (point-y p)]
            (- y (* m x))))))