;;
;; Exercise 1.3
;;
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of squares of the two largest numbers.
;;

;; define the "square" form
(defn square [n] (* n n))

;; define the "sum-of-squares" form
(defn sum-of-squares [x y]
  (+ (square x) (square y)))

;;
;; Procedure takes three numbers as arguments, and returns
;; the sum of the squares of the two largest numbers.
;; An "error" condition is indicated by returning -1
;; (which can never be a sum of real squares), although
;; we should never reach this point.
;;
(defn f [x y z]
  (def smallest (min x y z))
  (cond (= x smallest) (sum-of-squares y z)
        (= y smallest) (sum-of-squares x z)
        (= z smallest) (sum-of-squares x y)
        :else -1))