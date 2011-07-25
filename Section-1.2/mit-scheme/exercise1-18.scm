;;
;; Exercise 1.18
;;
;; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative
;; process for multiplying two integers in terms of adding, doubling, and halving and uses a 
;; logarithmic number of steps.
;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? n) (= (remainder n 2) 0))

(define (* a b)
  (*-iter a b 0))

(define (*-iter a b n)
  (cond ((= a 0) n)
	((even? a) (*-iter (halve a) (double b) n))
	(else
	 (*-iter (+ a -1) b (+ b n)))))
