;;
;; Exercise 05
;;
;; One last "expt" version. This time, both time and space must be less than O(n).
;;

;;
;; The idea is to take the procedure we defined in Exercise 4, but implement
;; it in a "tail-recursive" manner, so that it consumes only constant space.
;;

;;
;; The "old" procedure from Exercise 4:
;;
(defn expt [b n]
  (defn square [x] (* x x))
  (defn iter [k]
    (cond (= k 0) 1
          (even? k) (square (iter (quot k 2)))
          :else
          (* b (iter (- k 1)))))
  (iter n))

;;
;; The "new" procedure that consumes only constant space:
;;
(defn expt [b n]
  (defn square [x] (* x x))
  (defn iter [r c k]
    (cond (= k 0) r
          (even? k) (iter r (square c) (quot k 2))
          :else
          (iter (* r c) c (- k 1))))
  (iter 1 b n))

;;
;; Let's expand this procedure call using the substitution model:
;;
(expt 3 5)
(iter 1 3 5)
(iter 3 3 4)
(iter 3 9 2)
(iter 3 81 1)
(iter 243 81 0)
243

;;
;; That's a pretty fast evaluation!
;;

;;
;; The expression (expt 3 5) evaluated in 7 steps (in time), and 1 slot in space.
;;

;;
;; Let's see what happens if we try to double n, to n=10:
;;
(expt 3 10)
(iter 1 3 10)
(iter 1 9 5)
(iter 9 9 4)
(iter 9 81 2)
(iter 9 6561 1)
(iter 59049 6561 0)
59049

;;
;; Again, a very fast evaluation!
;;

;;
;; The expression (expt 3 10) evaluated in 8 steps (in time), and 1 slot in space.
;;

;;
;; Clearly, the procedure evaluates in O(log n) in time, and O(1) space.
;;
          