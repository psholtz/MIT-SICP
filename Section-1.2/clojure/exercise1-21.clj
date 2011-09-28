;;
;; Exercise 1.21
;;
;; Use the smallest-divisor procedure to find the smallest divisor of each
;; of the following numbers: 199, 1999, 19999
;;

;;
;; Define the procedures
;;
(defn square [n] (* n n))

(defn divides? [a b]
  (= (rem b a ) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

;;
;; Run the tests
;;
(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7