;;
;; Exercise 1.19
;;

(defun fib (n)
  (fib-iter 1 0 0 1 n))

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
	((even count)
	 (fib-iter a 
		   b
		   (sum-of-squares p q)
		   (+ (square q) (* 2 p q))
		   (/ count 2)))
	(t
	 (fib-iter (+ (* b q) (* a q) (* a p))
		   (+ (* b p) (* a q))
		   p
		   q
		   (- count 1)))))

;; define the "even" form
(defun even (n) (= (% n 2) 0))

;; define the "square" form
(defun square (n) (* n n))

;; define the "sum of squares" form
(defun sum-of-squares (a b) (+ (square a) (square b)))

;; 
;; Run some simple unit tests
;;
(= (fib 0) 0)
(= (fib 1) 1)
(= (fib 2) 1)
(= (fib 3) 2)
(= (fib 4) 3)
(= (fib 5) 5)
(= (fib 6) 8)
(= (fib 7) 13)
(= (fib 8) 21)
(= (fib 9) 34)
(= (fib 10) 55)

;;
;; This code will hang an interpreter using a naive recursive process
;;
(= (fib 93) (+ (fib 92) (fib 91)))