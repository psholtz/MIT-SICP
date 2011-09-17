;;
;; Exericse 1.38
;;
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus
;; Continuis, which included a continued fraction expansion for e-2, where e is the 
;; base of the natural logarithm. In this fraction, the N(i) are all 1, and the D(i)
;; are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... Write a program that uses your
;; "cont-frac" procedure from exercise 1.37 to approximate e, based on Euler's expansion.

;; 
;; Let's use the iterative definition of the "cont-frac" procedure, 
;; as this is going to be more efficient.
;;
(defun cont-frac (n d k)
  (defun term (i v)
    (/ (funcall n i) (+ (funcall d i) v)))
  (defun cont-frac-iter (i v)
    (cond ((= i 1) (term i v))
	  (t
	   (cont-frac-iter (- i 1) (term i v)))))
  (cont-frac-iter k 0))

;; 
;; Define the numerator and denominator procedures:
;;
(setq n (lambda (x) 1.0))
(setq d (lambda (x)
	  (let ((r (% (- x 2) 3))
		(n (floor (/ (- x 2) 3))))
	    (cond ((= r 0) (* (+ n 1.0) 2.0))
		  (t 1.0)))))

;;
;; The continued fraction is supposed to give an expression for e-2. 
;; Let's add 2 to the continued fraction to get e, and let's carry 
;; out the continued fraction for 10 iterations, to see how close 
;; we get to e:
;;
(+ 2 (cont-frac n d 10))
;; ==> 2.71828171828

;;
;; Looks pretty good!
;;

;;
;; Let's implement the same "test" procedure as in Exercise 1.37, 
;; to see how many iterations we have to carry out to get an 
;; approximation that is accurate to 4 decimal places:
;;
(defun test ()
  (setq tolerance 0.0001)
  (setq target e)
  (defun test-iter (k)
    (let ((value (+ 2 (cont-frac n d k))))
      (if (< (abs (- value target)) tolerance)
	  k
	(test-iter (+ k 1)))))
  (test-iter 1))
;; ==> 7

;;
;; That is, 7 iterations are required to get an approximation to e that
;; is accurate to 4 decimal places.
;;
(+ 2 (cont-frac n d 7))
;; ==> 2.71830985915

(abs (- e (+ 2 (cont-frac n d 7)))
;; ==> 2.80307e-05