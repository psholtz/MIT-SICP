;;
;; Exercise 2.1
;;
;; Define a better version of "make-rat" that handles both positive and negative argument.
;; "make-rat" should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.
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

;;
;; Run the unit tests:
;;
(print-rat (make-rat 1 2))
;; ==> 1/2

(print-rat (make-rat -1 2))
;; ==> -1/2

(print-rat (make-rat 1 -2))
;; ==> -1/2

(print-rat (make-rat -1 -2))
;; ==> 1/2

(print-rat (make-rat 0 2))
;; ==> 0/2

;;
;; Note that we're not testing for 0 in the denominator yet.
;;