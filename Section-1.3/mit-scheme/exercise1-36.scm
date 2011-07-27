;;
;; Exercise 1.36
;;
;; Modify "fixed-point" so that it prints the sequence of approximations it generates, using the "newline"
;; and "display" primitives shown in exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed 
;; point of x |--> log(1000)/log(x). (Use Scheme's primitive "log" procedure, which computes natural logarithms).
;; Compare the number of steps this takes with and without average damping. (Note that you cannot start 
;; "fixed-point" with a guess of 1, as this would cause division by log(1)=0).
;;

;; 
;; Define the modified "fixed-point" procedure:
;;
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;
;; Use it to calculate phi:
;;
(define phi (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
;;
;; ==> 1.
;; ==> 2.
;; ==> 1.5
;; ==> 1.6666666
;; ==> 1.6
;; ==> 1.625
;; ==> 1.615384
;; ==> 1.619047
;; ==> 1.617647
;; ==> 1.618182
;; ==> 1.617977
;; ==> 1.618056
;; ==> 1.618026
;; ==> 1.618037
;; ==> phi

;;
;; Use it to calculate x as given in the text:
;;
(define x (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))

      