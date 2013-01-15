;;
;; Exercise 1.8
;;
;; Newton's method for cube roots is based on the fact that if y is 
;; an approximation to the cube root of x, then a better approximation 
;; is given by the value
;;
;;       (x/y^2) + 2y 
;;       ------------ 
;;            3 
;;
;; Use this formula to implement a cube-root procedure analogous to 
;; the square-root procedure.
;;

;; define the "cube" form 
(define (cube x) (* x x x))

;;
;; Interface to "cube-root-iter" procedure.
;;
(define (cube-root x)
  (cube-root-iter 1.0 x))

;;
;; Iterative procedure that narrows down the cube root.
;;
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

;;
;; Use the "new" method of approximation based on fractional differences.
;;
(define (good-enough? guess x)
  (define tolerance 0.001)
  (< (abs (- (/ (cube guess) x) 1.0)) tolerance))

;;
;; Newton's method of approximation for cube roots.
;;
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))