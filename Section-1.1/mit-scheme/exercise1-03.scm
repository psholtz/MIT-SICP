;;
;; Exercise 1.3
;;
;; Define a procedure that takes three numbers as arguments 
;; and returns the sum of squares of the two largest numbers.
;;

;; define the "square" form
(define (square x) (* x x))

;; define the "sum-of-squares" form
(define (sum-of-squares x y) (+ (square x) (square y)))

;;
;; Procedure takes three numbers as arguments, and returns
;; the sum of the squares of the two largest numbers.
;; An "error" condition is indicated by returning -1 
;; (which can never be a sum of real squares), although 
;; we should never reach this point.
;;
(define (f x y z)
  (define smallest (min x y z))
  (cond ((eq? x smallest) (sum-of-squares y z))
	((eq? y smallest) (sum-of-squares x z))
	((eq? z smallest) (sum-of-squares x y))
	(else -1)))

