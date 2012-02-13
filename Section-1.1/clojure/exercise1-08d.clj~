;;
;; Exercise 1.8
;;
;; Although not required, I thought it would be useful
;; to have all the sqrt code bundled together in the scope
;; of a single block, as demonstrated in the text.
;;

;; define the "square" form
(defn square [n] (* n n))

;;
;; Block defining the square root procedure.
;;
(defn sqrt [x]
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