;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Code from text implementating rational number arithmetic.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

;; 
;; Add two rational numbers together.
;; 
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

;;
;; Subtract two rational numbers.
;;
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

;;
;; Multiply two rational numbers.
;;
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

;;
;; Divide two rational numbers.
;;
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

;;
;; Equality comparator.
;;
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; 
;; Constructor for building rational numbers.
;;
(define (make-rat n d) (cons n d))

;;
;; Extract the numerator.
;;
(define (numer x) (car x))

;;
;; Extract the denominator.
;;
(define (denom x) (cdr x))

;;
;; Implement pretty-printing for rational numbers.
;;
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
