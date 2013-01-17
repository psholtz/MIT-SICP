;;
;; Exercise 1.11
;;
;; A function f is defined by the rule that f(n) = n if n < 3, and 
;; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3. Write a procedure 
;; that computes f by means of a recursive process. Write a procedure 
;; that computes f by means of an iterative process.
;;

;;
;; f defined as an iterative process.
;;
;; Iteration is not necessary until argument is greater than 2.
;;
(define (f n)
  (if (< n 3)
      n 
      (f-iter 2 1 0 n)))

(define (f-iter a b c n)
  (if (< n 3)
      a
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (- n 1))))
 
;;
;; Run some simple unit tests:
;;
(= (f -1) -1)
(= (f 0) 0)
(= (f 1) 1)
(= (f 2) 2)
(= (f 3) 4)
(= (f 4) 11)
(= (f 5) 25)
(= (f 6) 59)
(= (f 7) 142)
(= (f 8) 335)
(= (f 9) 796)
(= (f 10) 1892)