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
(defn cube [x] (* x x x))

;;
;; Use the "new" method of approximation based on fractional differences.
;;
(defn good-enough? [guess x]
  (def tolerance 0.001)
  (< (Math/abs (- (/ (cube guess) x) 1.0)) tolerance))

;;
;; Newton's method of approximation for cube roots.
;;
(defn improve [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

;;
;; Iterative procedure that narrows down the cube root.
;;
(defn cube-root-iter [guess x]
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

;;
;; Interface to "cube-root-iter" procedure.
;;
(defn cube-root [x]
  (cube-root-iter 1.0 x))