;;
;; Exercise 1.8
;;
;; Although not required, I thought it would be useful
;; to have all the sqrt code bundled together in the scope
;; of a single block, as demonstrated in the text.
;;

;; define the "square" form
(defn square
  {:doc "Return square of argument"}
  [n] (* n n))

;;
;; Block defining the square root procedure.
;;
(defn sqrt
  {:doc "Wrapper function to calculate (sqrt x)"}
  [x]
  (def tolerance 0.001)
  (defn good-enough? [guess]
    (< (Math/abs (- (/ (square guess) x) 1.0)) tolerance))
  (defn average [x y]
    (/ (+ x y) 2.0))
  (defn improve [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

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