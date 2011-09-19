;;
;; Exercise 2.10
;;
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments
;; that it is not clear what it means to divide by an interval that spans zero. Modify
;; Alyssa's code to check for this condition and to signal an error if it occurs.
;;

;;
;; The original "div-interval" code given by Alyssa:
;;
(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;
;; Modified version that checks for division by zero.
;;
;; Since we use the forms (/ 1.0 (upper-bound y)) and (/ 1.0 (lower-bound y))
;; in forming the result for "div-interval", and since we cannot divide by zero,
;; we will signal an error if either (lower-bound y) or (upper-bound y) is zero.
;;
(define (div-interval x y)
  (let ((p1 (lower-bound y))
	(p2 (upper-bound y)))
    (cond ((or (= p1 0) (= p2 0))
	   (display "*** Cannot divide by zero!"))
	  (else
	   (mul-interval x
			 (make-interval (/ 1.0 (upper-bound y))
					(/ 1.0 (lower-bound y))))))))