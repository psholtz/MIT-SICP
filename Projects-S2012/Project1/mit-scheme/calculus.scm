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
 
;; ++++++++++++++++++++++++++++++++ 
;; Problem 3
;; 
;; Integrating Bitdiddle's Function
;; ++++++++++++++++++++++++++++++++ 

;;
;; Note that if f(x) = x^4 + x^2 - 14, then int f(x) = (1/5)x^5 + (1/3)x^3 - 14x.
;;

;;
;; Recursive function definition:
;; (use decimals)
;;
(define (bitfunc-integral-recur num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x)
      (if (>= (+ x dx) x2)
	  (bitfunc-rect x (+ x dx))
	  (+ (bitfunc-rect x (+ x dx)) (integrate (+ x dx)))))
    (integrate x1)))

;;
;; Let's see if "step-wise" (for large steps) we can what we expect:
;;
(= (bitfunc-integral-recur 1 0 1) (bitfunc 0))
;; ==> #t
(= (bitfunc-integral-recur 2 0 1) (/ (+ (bitfunc 0.0) (bitfunc 0.5)) 2.0))
;; ==> #t
(= (bitfunc-integral-recur 4 0 1) (/ (+ (bitfunc 0.0) (bitfunc 0.25) (bitfunc 0.5) (bitfunc 0.75)) 4.0))
;; ==> #t

;;
;; Looks OK, let's see if our definite integrals evaluate to something close to their expected values:
;;
;; Int f(x) from 0 to 1 should work out to (+ (/ 1 5) (/ 1 3) -14) = -13.466666666 or -13 21/45
;;
(bitfunc-integral-recur 10 0 1)
;; ==> -14.76167
(bitfunc-integral-recur 100 0 1)
;; ==> -13.476616667
(bitfunc-integral-recur 1000 0 1)
;; ==> -13.46766616666667

;;
;; Looks like the approximations are pretty good.
;;

;; [WORKING --> do more integrals here]

;;
;; Iterative function definition:
;;
(define (bitfunc-integral-iter num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x total)
      (if (>= (+ x dx) x2)
	  (+ (bitfunc-rect x (+ x dx)) total)
	  (+ (bitfunc-rect x (+ x dx)) (integrate (+ x dx) total))))
    (integrate x1 0.0)))

;;
;; For the unit tests, we can simply verify that the recursive version gives 
;; the same answer as the iterative version.
;;
(= (bitfunc-integral-iter 10 0 1) (bitfunc-integral-recur 10 0 1))
;; ==> #t
(= (bitfunc-integral-iter 100 0 1) (bitfunc-integral-recur 100 0 1))
;; ==> #t
(= (bitfunc-integral-iter 1000 0 1) (bitfuc-integral-recur 1000 0 1))
;; ==> #t

;; [WORKING --> more definite integrals]

;; ++++++++++++++++++++++++ 
;; Problem 4
;; 
;; Integrating any function
;; ++++++++++++++++++++++++ 

;; 
;; Iterative (i.e., tail-recursive) is more efficient, so let's use that model:
;;
(define (integral func num-steps x1 x2)
  ;; calculate the area under the functions
  (define (func-rect x1 x2)
    (let ((dx (- x2 x1)))
      (* (func x1) dx)))

  ;; define the recursive process to numerically calculate integral
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x total)
      (if (>= (+ x dx) x2)
	  (+ (func-rect x (+ x dx)) total)
	  (+ (func-rect x (+ x dx)) (integrate (+ x dx) total))))
    (integrate x1 0.0)))

;;
;; Define some procedures, and calculate their integrals:
;;
(define (square x) (* x x))
(define (cube x) (* x x))

;;
;; For reference:
;;
;; Int square(x) = (1/3) x^3
;; Int cube(x) = (1/4) x^4
;; Int sin(x) = -cos(x)
;;

;;
;; Run some unit tests:
;;
(integral square 10 0 1)
;; ==> 0.385
(integral square 100 0 1)
;; ==> 0.32835
(integral square 1000 0 1)
;; ==> 0.3328335
(integral square 10000 0 1)
;; ==> 0.33383335

;;
;; The answer clearly converges to the proper answer, but it takes 10,000 steps to get 
;; 3 digits of accuracy. Perhaps we could do better with a different integration algorithm.
;;

(integral cube 10 0 1)
;; ==> 0.3025
(integral cube 100 0 1)
;; ==> 0.245025
(integral cube 1000 0 1)
;; ==> 0.24950025
(integral cube 10000 0 1)
;; ==> 0.2500500025

;;
;; Again, the returned values converge to the expect answer, but the converge is rather slow.
;;
(define pi (* 4 (atan 1.0)))
;; ==> 3.14159...

(integral sin 10 0 pi)
;; ==> 1.983523...
(integral sin 100 0 pi)
;; ==> 1.9998355...
(integral sin 1000 0 pi)
;; ==> 1.99999835...
(integral sin 10000 0 pi)
;; ==> 1.99999999...

;;
;; Here the convergence seems to be somewhat better than for the polynomial functions.
;;

;; +++++++++++++++++++++ 
;; Problem 5
;;
;; Area of a unit circle
;; +++++++++++++++++++++ 
(define (approx-pi num-steps)
  (* 4 (integral (lambda (x) (sqrt (- 1 (square x)))) num-steps 0 1)))

(approx-pi 10)
;; ==> 3.30451883322...
(approx-pi 100)
;; ==> 3.16041703...
(approx-pi 1000)
;; ==> 3.143555467...
(approx-pi 10000)
;; ==> 3.1417914777...

;;
;; Again, the response converges to the proper answer, but the convergence seems rather slow.
;;

;; ++++++++++++++++++++++++++++++++++++ 
;; Problem 6  
;; 
;; Integrating with pieces of any shape
;; ++++++++++++++++++++++++++++++++++++ 
;; [WORKING --> not quite right yet]
;; area under rectangle
(define (rectangle func x1 x2)
  (let ((dx (- x2 x1)))
    (* (func x1) dx)))

;; are under trapezoid
(define (trapezoid func x1 x2)
  (let ((dx (- x2 x1)))
    (* dx (/ (+ (func x1) (func x2)) 2.0))))

(define (integral-with piece func num-steps x1 x2)
  (let ((dx (* 1.0 (/ (- x2 x1) num-steps))))
    (define (integrate x total)
      (let ((value (+ (piece func x (+ x dx)))))
	(if (>= (+ x dx) x2)
	    (+ value total)
	    (+ value (integrate (+ x dx) total)))))
    (integrate x1 0)))

;;
;; Check that "integral-with rectangle" works out to the same answer that the
;; original "integral" procedure produced.
;;
(= (integral square 10 0 1) (integral-with rectangle square 10 0 1))
;; ==> #t
(= (integral square 100 0 1) (integral-with rectangle square 100 0 1))
;; ==> #t
(= (integral square 1000 0 1) (integral-with rectangle square 1000 0 1))
;; ==> #t

(= (integral cube 10 0 1) (integral-with rectangle cube 10 0 1))
;; ==> #t
(= (integral cube 100 0 1) (integral-with rectangle cube 100 0 1))
;; ==> #t
(= (integral cube 1000 0 1) (integral-with rectangle cube 1000 0 1))
;; ==> #t


;; ++++++++++++++++++++++++++ 
;; Problem 7
;;
;; Better approximation of pi
;; ++++++++++++++++++++++++++
(define (better-pi num-steps)
  '())