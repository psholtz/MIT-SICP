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
  (fixed-point (average-damp 
		(average-damp 
		 (average-damp (lambda (y) (/ x (n-th-power y 7))))))
	       1.0))

;;
;; ...
;;

(define (fifteenth-root x)
  (fixed-point (average-damp
		(average-damp
		 (average-damp (lambda (y) (/ x (n-th-power y 14))))))
	       1.0))

;; 
;; The following procedure requires four calls to "average-damp":
;;
(define (sixteenth-root x)
  (fixed-point (average-damp
		(average-damp
		 (average-damp
		  (average-damp 
		   (lambda (y) (/ x (n-th-power y 15)))))))
	       1.0))

;;
;; A pattern suggests itself:
;; 
;; n = 2 or n = 3 ==> apply "average-damp" 1x
;; n = 4 or n = 5 or n = 6 or n = 7 ==> apply "average-damp" 2x
;; n = 8 or n = 9 or n = 10 or n = 11 
;; or n = 12 or n = 13 or n = 14 or n = 15 ==> apply "average-damp" 3x
;; n = 16 ... ==> apply "average-damp" 4x
;;
;; The function of n we are looking for, to give the number of times 
;; we should apply "average-damp", is given by:
;;
;; (floor (/ (log n) (log 2)))
;;
(= 1 (floor (/ (log 2) (log 2))))
(= 1 (floor (/ (log 3) (log 2))))
(= 2 (floor (/ (log 4) (log 2))))
(= 2 (floor (/ (log 5) (log 2))))
(= 2 (floor (/ (log 6) (log 2))))
(= 2 (floor (/ (log 7) (log 2))))
(= 3 (floor (/ (log 8) (log 2))))
(= 4 (floor (/ (log 16) (log 2))))

;;
;; Let's define the "repeated" procedure from the previous exercises, 
;; so we can use that in our procedure definition:
;;
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter g c)
    (cond ((>= c n) g)
	  (else
	   (repeated-iter (compose g f) (+ c 1)))))
  (repeated-iter f 1))

;;
;; Finally define the n-th-root procedure that we are seeking:
;;
(define (n-th-root x n)
  (let ((k (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp k)
		  (lambda (y) (/ x (n-th-power y (- n 1)))))
		 1.0)))