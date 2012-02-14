;;
;; Exercise 1.18
;;
;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative
;; process for multiplying two integers in terms of adding, doubling, and halving and uses a
;; logarithmic number of steps.
;;
(defn double
  {:doc "Double the argument"}
  [x] (+ x x))

(defn halve
  {:doc "Divide the argument in half"}
  [x] (/ x 2))

(defn *-iter
  {:doc "Iterative procedure for doing custom multiplications"}
  [a b n]
  (cond (= a 0) n
        (even? a) (*-iter (halve a) (double b) n)
        :else (*-iter (+ a -1) b (+ b n))))

(defn *
  {:doc "Wrapper procedure for multiplying two numbers in an iterative fashion"}
  [a b]
  (*-iter a b 0))
  