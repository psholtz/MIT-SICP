;;
;; Exercise 1.3
;;
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of squares of the two largest numbers.
;;

;; define the "square" form
(defn square
  {:doc "Return the square of the argument"}
  [n] (* n n))

;; define the "sum-of-squares" form
(defn sum-of-squares
  {:doc "Return the sum of the squares of the arguments"}
  [x y] (+ (square x) (square y)))

;;
;; Procedure takes three numbers as arguments, and returns
;; the sum of the squares of the two largest numbers.
;; An "error" condition is indicated by returning -1
;; (which can never be a sum of real squares), although
;; we should never reach this point.
;;
(defn f
  {:doc "Take three arguments, and return the sum of the squares of the two largest"}
  [x y z]
  (def smallest (min x y z))
  (cond (= x smallest) (sum-of-squares y z)
        (= y smallest) (sum-of-squares x z)
        (= z smallest) (sum-of-squares x y)
        :else -1))

;;
;; Run unit tests:
;;
(= (f 1 1 1) 2)
(= (f 1 1 2) 5)
(= (f 1 2 1) 5)
(= (f 2 1 1) 5)
(= (f 1 2 3) 13)
(= (f 1 3 2) 13)
(= (f 2 1 3) 13)
(= (f 2 3 1) 13)
(= (f 3 1 2) 13)
(= (f 3 2 1) 13)

