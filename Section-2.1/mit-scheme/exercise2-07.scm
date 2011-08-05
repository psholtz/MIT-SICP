;;
;; Exercise 2.7
;;
;; 

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))