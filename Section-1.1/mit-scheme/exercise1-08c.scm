;;
;; Exercise 1.8
;;
;; Although not requested, I thought it would be useful to 
;; bundle all the cube-root code together into a single block, 
;; as demonstrated in the text.
;;

;; define the "cube" form
(define (cube x) (* x x x))

;;
;; Block defining the cube root procedure.
;;
(define (cube-root x)
  (define tolerance 0.001)
  (define (good-enough? guess) 
    (< (abs (- (/ (cube guess) x) 1.0)) tolerance))
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
	guess
	(cube-root-iter (improve guess))))
  (cube-root-iter 1.0))

