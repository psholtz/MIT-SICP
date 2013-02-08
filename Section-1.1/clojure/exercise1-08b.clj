;;
;; Exercise 1.8
;;
;; Although not requested, I thought it's useful to have
;; the sqrt code all together in one place.
;;

;; define the "square" form
(defn square
  {:doc "Return square of the argument"}
  [n] (* n n))

;; define supporting procedures
(defn average
  {:doc "Return the average of x and y"}
  [x y]
  (/ (+ x y) 2.0))

(defn improve
  {:doc "Make an improvement to guess, relative to x"}
  [guess x]
  (average guess (/ x guess)))

;;
;; Use the "new" method of approximation, based on fractional changes.
;;
(defn good-enough?
  {:doc "Is guess a good enough approximation to (sqrt x)"}
  [guess x]
  (def tolerance 0.001)
  (< (Math/abs (- (/ (square guess) x) 1.0)) tolerance))

;;
;; Define iterative procedure for arriving at square roots.
;;
(defn sqrt-iter
  {:doc "Make an iterative improvement to (sqrt x), starting from guess"}
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;;
;; Define interface to "sqrt-iter" procedure
;;
(defn sqrt
  {:doc "Wrapper function to calculate (sqrt x)"}
  [x]
  (sqrt-iter 1.0 x))

;;
;; Run some unit tests:
;;
(sqrt 1)
;; ==> 1.0

(sqrt 2)
;; ==> 1.4142156862745097

(sqrt 3)
;; ==> 1.7321428571428572

(sqrt 4)
;; ==> 2.000609756097561