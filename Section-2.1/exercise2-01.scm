;;
;; Exercise 2.1
;;

;;
;; Modified version of the "make-rat" constructor:
;;
(define (make-rat n d) 
  (define (positive? x)
    (> x 0))
  (define (negative? x)
    (< x 0))
  (if (negative? d)
      (cons (* -1 n) (* -1 d))
      (cons n d)))

;;
;; Let's define the "print-rat" procedure so we can run some unit tests.
;;
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;
;; Need selectors for numerator and denominator as well:
;;
(define (numer x) (car x))
(define (denom x) (cdr x))
