;;
;; Exercise 1.8
;;
;; Although not requested, I thought it would be useful to
;; bundle all the cube-root code together into a single block,
;; as demonstrated in the text.
;;

;; define the "cube" form
(defn cube [n] (* n n n))

;;
;; Block defining the cube root procedure.
;;
(defn cube-root [x]
  (def tolerance 0.001)
  (defn good-enough? [guess]
    (< (Math/abs (- (/ (cube guess) x) 1.0)) tolerance))
  (defn improve [guess]
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (defn cube-root-iter [guess]
    (if (good-enough? guess)
      guess
      (cube-root-iter (improve guess))))
  (cube-root-iter 1.0))