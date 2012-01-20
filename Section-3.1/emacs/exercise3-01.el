;;
;; Exercise 3.1
;;
;; An "accumulator" is a procedure that is called repeatedly with a single numeric argument
;; and accumulates its argument into a sum. Each time it is called, it returns the currently
;; accumulated sum. Write a procedure "make-accumulator" that generates accumulators, each
;; maintaining an independent sum. The input to "make-accumulator" should specify the value
;; of the sum; for example
;;
;; (define A (make-accumulator 5))
;; (A 10)
;; ==> 15
;; (A 10)
;; ==> 25
;;

;; require lexical scoping
(require 'cl)

;; define the procedure
(defun make-accumulator (balance)
  (lexical-let ((foo balance))
    (lambda (amount)
      (setq foo (+ foo amount))
      foo)))

;; run the unit tests 
(setq A (make-accumulator 5))

(funcall A 10)
;; --> 15

(funcall A 10)
;; --> 25