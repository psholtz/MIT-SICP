;; ++++++++++++++++++++ 
;; Problem 1
;; 
;; Bitdiddle's Function
;; ++++++++++++++++++++ 

;;
;; Define the function f(x) = x^4 + x^2 - 14
;;
(define (bitfunc x)
  (+ 
   (expt x 4)
   (expt x 2)
   -14))

;;
;; Run some unit tests:
;;
(= (bitfunc -2) 6)
;; ==> #t
(= (bitfunc -1) -12)
;; ==> #t
(= (bitfunc 0) -14)
;; ==> #t
(= (bitfunc 1) -12)
;; ==> #t
(= (bitfunc 2) 6)
;; ==> #t
(= (bitfunc 5) 636)
;; ==> #t

;; +++++++++++++++++ 
;; Problem 2
;;
;; Area of Rectangle
;; +++++++++++++++++ 
;; (assume that x2 > x1 always)
(define (bitfunc-rect x1 x2)
  (let ((dx (- x2 x1)))
    (* (bitfunc x1) dx)))

;;
;; Run some unit tests:
;;
(= (bitfunc-rect -2 -1) 6)
;; ==> #t
(= (bitfunc-rect -2 0) 12)
;; ==> #t
(= (bitfunc-rect -2 1) 18)
;; ==> #t

(= (bitfunc-rect -1 0) -12)
;; ==> #t
(= (bitfunc-rect -1 1) -24)
;; ==> #t
(= (bitfunc-rect -1 2) -36)
;; ==> #t

(= (bitfunc-rect 0 1) -14)
;; ==> #t
(= (bitfunc-rect 0 2) -28)
;; ==> #t
(= (bitfunc-rect 0 3) -42)
;; ==> #t

(= (bitfunc-rect 1 2) -12)
;; ==> #t
(= (bitfunc-rect 1 3) -24)
;; ==> #t 
(= (bitfunc-rect 1 4) -36)
;; ==> #t

(= (bitfunc-rect 2 3) 6)
;; ==> #t
(= (bitfunc-rect 2 4) 12)
;; ==> #t
(= (bitfunc-rect 2 5) 18)
;; ==> #t

 
;; [working]

;; ++++++++++++++++++++++++++++++++ 
;; Problem 3
;; 
;; Integrating Bitdiddle's Function
;; ++++++++++++++++++++++++++++++++ 

;;
;; Recursive function definition:
;; (working --> adjust conditional)
;;
(define (bitfunc-integral-recur num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x)
      (if (>= x x2)
	  (* (bitfunc x) dx)
	  (+ (* (bitfunc x) dx) (integrate (+ x dx)))))
    (integrate x1)))

;;
;; Run some unit tests:
;;

;; [working]

;;
;; Iterative function definition:
;; (working --> adjust conditional)
;;
(define (bitfunc-integral-iter num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x total)
      (let ((value (bitfunc x)))
	(if (>= x x2)
	    (+ value total)
	    (integrate (+ x dx) (+ total value)))))
    (integrate x1 0.0)))

;;
;; Run some unit tests:
;;

;; [working]

;; ++++++++++++++++++++++++ 
;; Problem 4
;; 
;; Integrating any function
;; ++++++++++++++++++++++++ 
;; (working --> check the bounds)
(define (integral func num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-stemps))))
    (define (integrate x total)
      (let ((value (func x)))
	(if (>= x x2)
	    (+ value total)
	    (integrate (+ x dx) (+ total value)))))
    (integrate x1 0.0)))


;; +++++++++++++++++++++ 
;; Problem 5
;;
;; Area of a unit circle
;; +++++++++++++++++++++ 
(define (square x) (* x x))
(define (approx-pi num-steps)
  (* 
   4
   (integral (lambda (x) (sqrt (- 1 (square x)))) num-steps 0 1)))

;; ++++++++++++++++++++++++++++++++++++ 
;; Problem 6  
;; 
;; Integrating with pieces of any shape
;; ++++++++++++++++++++++++++++++++++++ 
;; area under rectangle
(define (rectangle func x1 x2)
  (let ((dx (- x2 x1)))
    (* (func x1) dx)))

;; are under trapezoid
(define (trapezoid func x1 x2)
  (let ((dx (- x2 x1)))
    (* dx (/ (+ (func x1) (func x2)) 2.0))))

(define (integral-with piece func num-steps x1 x2)
  '())

;; ++++++++++++++++++++++++++ 
;; Problem 7
;;
;; Better approximation of pi
;; ++++++++++++++++++++++++++
(define (better-pi num-steps)
  '())