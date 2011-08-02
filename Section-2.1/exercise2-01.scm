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
  (cond ((and (positive? n) (positive? d)) (cons n d))
	((and (negative? n) (negative? d)) (cons (* -1 n) (* -1 d)))
	((and (positive? n) (negative? d)) (cons (* -1 n) (* -1 d)))
	((and (negative? n) (positive? d)) (cons n d))
	(else
	 (cons n d))))