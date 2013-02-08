;;
;; Exercise 1.8
;;
;; Newton's method for cube roots is based on the fact that if y is
;; an approximation to the cube root of x, then a better approximation
;; is given by the value
;;
;;       (x/y^2) + 2y
;;       ------------
;;            3
;;
;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure.
;;

;; define the "cube" form
(defn cube
  {:doc "Return the cube of the argument"}
  [x] (* x x x))

;;
;; Use the "new" method of approximation based on fractional differences.
;;
(defn good-enough?
  {:doc "Is (cube guess) close enough to x for our tolerance?"}
  [guess x]
  (def tolerance 0.001)
  (< (Math/abs (- (/ (cube guess) x) 1.0)) tolerance))

;;
;; Newton's method of approximation for cube roots.
;;
(defn improve
  {:doc "Improve guess so that it comes closer to (cube-root x)"}
  [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

;;
;; Iterative procedure that narrows down the cube root.
;;
(defn cube-root-iter
  {:doc "Make an iterative improvement to our approximation for (cube-root x)"}
  [guess x]
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

;;
;; Interface to "cube-root-iter" procedure.
;;
(defn cube-root
  {:doc "Wrapper function to calculate (cube-root x)"}
  [x]
  (cube-root-iter 1.0 x))

;;
;; Run some unit tests:
;;
(cube-root 2)
;; ==> 1.4422497895989996

(cube-root 3)
;; ==> 1.259933493449977

(cube-root 8)
;; ==> 2.000004911675504