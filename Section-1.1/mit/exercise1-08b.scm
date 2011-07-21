;;
;; Exercise 1.8
;;
;; Although not requested, I thought it's useful to have 
;; the sqrt code all together in one place.
;;

;; define the "square" form
(define (square x) (* x x))

;;
;; Define interface to "sqrt-iter" procedure.
;;
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;
;; Define iterative procedure for arriving at square roots.
;;
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;; 
;; Use the "new" method of approximation, based on fractional changes.
;;
(define (good-enough? guess x)
  (define tolerance 0.001)
  (< (abs (- (/ (square guess) x) 1.0)) tolerance))

;;
;; Supporting procedures 
;;
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

