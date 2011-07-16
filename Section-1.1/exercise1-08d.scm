;;
;; Exercise 1.8
;;
;; Although not required, I thought it would be useful
;; to have all the sqrt code bundled together in the scope
;; of a single block, as demonstrated in the text.
;;

;; define the "square" form
(define (square x) (* x x))

;;
;; Block defining the square root procedure.
;;
(define (sqrt x)
  (define tolerance 0.001)
  (define (good-enough? guess)
    (< (abs (- (/ (square guess) x) 1.0)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average a b)
    (/ (+ a b) 2))
  (define (sqrt-iter guess)
    (if (good-enough? guess) 
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
