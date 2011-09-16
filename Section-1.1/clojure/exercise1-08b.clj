;;
;; Exercise 1.8
;;
;; Although not requested, I thought it's useful to have
;; the sqrt code all together in one place.
;;

;; define the "square" form
(defn square [n] (* n n))

;; define supporting procedures
(defn average [x y]
  (/ (+ x y) 2.0))

(defn improve [guess x]
  (average guess (/ x guess)))

;;
;; Use the "new" method of approximation, based on fractional changes.
;;
(defn good-enough? [guess x]
  (def tolerance 0.001)
  (< (Math/abs (- (/ (square guess) x) 1.0)) tolerance))

;;
;; Define iterative procedure for arriving at square roots.
;;
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;;
;; Define interface to "sqrt-iter" procedure
;;
(defn sqrt [x]
  (sqrt-iter 1.0 x))