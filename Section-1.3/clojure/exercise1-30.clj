;;
;; Exercise 1.30
;;

;;
;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;;  (sum term (next a) next b))))
;;

;;
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that
;; the sum is performed iteratively. Show how to do this by filling in the missing expressions:
;;
(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

;;
;; We can unit test the procedure as follows:
;;
(defn cube [x] (* x x x))
(defn sum-cubes [a b]
  (sum cube a inc b))

(sum-cubes 1 10)
;; ==> 3025