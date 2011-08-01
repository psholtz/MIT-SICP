;;
;; Exercise 1.45
;;

;;
;; Define the "fixed-point" procedure:
;;
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;
;; Define the "average damping" procedures:
;;
(define (average x y) (/ (+ x y) 2.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;
;; Define a utility procedure that allows us to raise the number x to the n-th power:
;;
(define (n-th-power x n)
  (define (n-th-power-iter c t)
    (if (= c n)
	t
	(n-th-power-iter (+ c 1) (* t x))))
  (n-th-power-iter 0 1))

;;
;; Run some unit tests of "n-th-power":
;;
(n-th-power 2 0)
;; ==> 1

(n-th-power 2 1)
;; ==> 2

(n-th-power 2 2)
;; ==> 4

(n-th-power 2 10)
;; ==> 1024

(n-th-power 3 2)
;; ==> 9

(n-th-power 3 4)
;; ==> 81

;; 
;; These procedures require one call to "average-damp":
;;
(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x (n-th-power y 1))))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (n-th-power y 2))))
	       1.0))

;;
;; These procedures require two calls to "average-damp":
;;
(define (fourth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (n-th-power y 3)))))
	       1.0))

(define (fifth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (n-th-power y 4)))))
	       1.0))

(define (sixth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (n-th-power y 5)))))
	       1.0))

(define (seventh-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (n-th-power y 6)))))
	       1.0))

;;
;; These procedures require three calls to "average-damp":
;;
(define (eighth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (n-th-power y 7)))))
	       1.0))





