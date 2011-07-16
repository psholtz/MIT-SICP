;;
;; Exercise 3.1
;;

(define (make-accumulator balance)
  (lambda (amount)
    (set! balance (+ balance amount))
    balance))

(define A (make-accumulator 5))
;; --> A

(A 10)
;; --> 15

(A 10)
;; --> 25