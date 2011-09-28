;;
;; Exercise 1.18
;;
;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative
;; process for multiplying two integers in terms of adding, doubling, and halving and uses a
;; logarithmic number of steps.
;;
(defn double [x] (+ x x))

(defn halve [x] (/ x 2))

(defn *-iter [a b n]
  (cond (= a 0) n
        (even? a) (*-iter (halve a) (double b) n)
        :else (*-iter (+ a -1) b (+ b n))))

(defn * [a b]
  (*-iter a b 0))
  