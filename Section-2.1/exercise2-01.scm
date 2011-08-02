;;
;; Exercise 2.1
;;

;;
;; First let's set up the procedures we need to support rational arithmetic. 
;;

;;
;; Constructors and selectors
;;
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;;
;; Output to console
;;
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;
;; Arithmetic operations
;;
(define (add-rat x y)
  (make-rat (+ 
	     (* (numer x) (denom y))
	     (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-
	     (* (numer x) (denom y))
	     (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))