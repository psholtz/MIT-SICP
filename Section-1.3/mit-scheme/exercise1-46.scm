;;
;; Exercise 1.46
;;

(define (iterative-improvement good-enough? improve)
  (lambda (first-guess)
    (define (iteration guess)
      (if (good-enough? guess)
	  guess
	  (iteration (improve guess)))))
  (iteration first-guess))

(define (sqrt x)
  (define tolerance 0.00001)
  (define (average x y) (/ (+ x y) 2.0))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improvement good-enough? improve) 1.0))


