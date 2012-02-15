;;
;; Exercise 2
;;
;; Write a procedure that computes the factorial of a number n.
;;
(defn fact [n]
  (cond (= n 0) 1
        :else
        (* n (fact (- n 1)))))

;;
;; Unit tests:
;;
(fact 0)
;; ==> 1

(fact 1)
;; ==> 1

(fact 2)
;; ==> 2

(fact 3)
;; ==> 6

(fact 4)
;; ==> 24

(fact 5)
;; ==> 120