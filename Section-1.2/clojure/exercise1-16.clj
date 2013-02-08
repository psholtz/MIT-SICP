;;
;; Exercise 1.16
;;
;; Design a procedure that evolves an iterative exponentiation process that uses successive squaring
;; and uses a logarithmic number of steps, as does "fast-expt". (Hint: Using the observation that
;; (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent n and the base b, an additional state
;; variable a, and define the state transformation in such a way that the product a b^n is unchanged
;; from state to state. At the beginning of the process a is taken to be 1, and the answer is given
;; by the value of a at the end of the process. In general, the technique of defining an invariant
;; quantity that remains unchanged from state to state is a powerful way to think about the design
;; of iterative algorithms.)
;;

(defn square
  {:doc "Return the square of the argument"}
  [n] (* n n))

(defn fast-expt-iter
  {:doc "Iterative procedure for (fast) calculation of b^n"}
  [b n a]
  (cond (= n 0) a
        (even? n) (fast-expt-iter (square b) (/ n 2) a)
        :else (fast-expt-iter b (- n 1) (* a b))))

(defn fast-expt
  {:doc "Wrapper function for procedure for (fast) calculation of b^n"}
  [b n]
  (fast-expt-iter b n 1))

;;
;; Run some unit tests:
;;
(fast-expt 2 3)
;; ==> 8

(fast-expt 3 4)
;; ==> 81

(fast-expt 17 11)
;; ==> 34271896307633