;;
;; Alyssa's representation of complex numbers.
;; She has chosen to use the polar representation.
;;
(define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
(define (make-from-mag-ang r a) (cons r a)
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))