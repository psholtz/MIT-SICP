;;
;; Exercise 1.7
;;
;; The good-enough? test used in computing square roots will not be very 
;; effective for finding the square roots of very small numbers. Also, 
;; real computers, arithmetic operations are almost always performed with 
;; limited precision. This makes our test inadequate for very large numbers.
;; Explain these statements, with examples showing how the test fails for small
;; and large numbers. An alternative strategy for implementing good-enough?
;; is to watch how guess changes from one iteration to the next and to stop
;; when the change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better for small
;; and large numbers?
;;

;; 
;; The procedure fails for small numbers because the tolerance is too coarse.
;;
;; For instance, the procedure will return 0.03125 as an "answer" to 
;; (sqrt 1e-10), since (< (abs (- (square 0.03125) 1e-10)) 0.001) returns true, 
;; even though 0.03125 is clearly not a very good approximation to the 
;; correct answer 1e-05.
;;
;; Similarly, the procedure fails for large numbers because the tolerance is too fine.
;;
;; For instance, within the limits of machine precision, the procedure will never
;; return from (sqrt 2e16), since the difference between the approximation arrived
;; at by the procedure and the actual answer is never less than 0.001.
;;
;; A better implementation would watch how guess changes from one iteration 
;; to the next, and stops when the change is a very small fraction of guess.
;; This implementation is given in the following code:
;;

;; define the "square" form
(define (square x) (* x x))

;; implementation of "sqrt" procedure
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; 
;; Modified version of good-enough?, based on fractional
;; changes, rather than on an absolute tolerance. 
;; The fractional tolerance is set at 0.1%.
;;
(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) 1.0)) 0.001))

;;
;; Run some unit tests on large and small numbers:
;;
(sqrt 1)
;; ==> 1.0

(sqrt 2)
;; ==> 1.4142156862756097

(sqrt 3)
;; ==> 1.7321428571428572

(sqrt 4)
;; ==> 2.000609756097561

(sqrt 9)
;; ==> 3.00009155413138

(sqrt 0.01)
;; ==> 0.10000052895642693

(sqrt 0.0001)
;; ==> 1.0000714038711746e-2