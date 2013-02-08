;;
;; Exercise 1.8
;;
;; Although not requested, I thought it would be useful to
;; bundle all the cube-root code together into a single block,
;; as demonstrated in the text.
;;

;; define the "cube" form
(defn cube
  {:doc "Return cube of the argument"}
  [n] (* n n n))

;;
;; Block defining the cube root procedure.
;;
(defn cube-root
  {:doc "Wrapper function to calculate cube-root of x"}
  [x]
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

;;
;; Run some unit tests:
;;
(cube-root 2)
;; ==> 1.259933493449977

(cube-root 3)
;; ==> 1.4422497895989996

(cube-root 8)
;; ==> 2.000004911675504