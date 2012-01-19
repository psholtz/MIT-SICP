;; ++++++++++++++++++++ 
;; Problem 1
;; 
;; Bitdiddle's Function
;; ++++++++++++++++++++ 
(define (bitfunc x)
  (+ 
   (expt x 4)
   (expt x 2)
   -14))

;;
;; Run some unit tests:
;;

;; [working]

;; +++++++++++++++++ 
;; Problem 2
;;
;; Area of Rectangle
;; +++++++++++++++++ 
(define (bitfunc-rect x1 x2)
  (let ((dx (- x2 x1)))
    (* (bitfunc x1) dx)))

;;
;; Run some unit tests:
;;

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

