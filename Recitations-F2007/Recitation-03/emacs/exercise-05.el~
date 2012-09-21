;;
;; Exercise 5
;;
;; Write a procedure "fib" that computes the n-th Fibonacci number.
;;
(defun fib (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t
	 (+ (fib (- n 1)) (fib (- n 2))))))

;;
;; Run the unit tests
;;
(fib 0)
;; ==> 1

(fib 1)
;; ==> 1

(fib 2)
;; ==> 2

(fib 3)
;; ==> 3

(fib 4)
;; ==> 5

(fib 5)
;; ==> 8