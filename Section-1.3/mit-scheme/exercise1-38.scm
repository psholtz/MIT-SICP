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
(define (cont-frac n d k)
  (define (term i t)
    (/ (n i) (+ (d i) t)))
  (define (cont-frac-iter i t)
    (cond ((= i 1) (term i t))
	  (else
	   (cont-frac-iter (- i 1) (term i t)))))
  (cont-frac-iter k 0))

;; 
;; Define the numerator and denominator procedures:
;;
(define n (lambda (x) 1.0))
(define d (lambda (x)
	    (let ((r (remainder (- x 2) 3))
		  (n (floor (/ (- x 2) 3))))
	      (cond ((= r 0) (* (+ n 1.0) 2.0))
		    (else 1.0)))))

;;
;; The continued fraction is supposed to give an expression for e-2. 
;; Let's add 2 to the continued fraction to get e, and let's carry 
;; out the continued fraction for 10 iterations, to see how close 
;; we get to e:
;;
(+ 2 (cont-frac n d 10))
;; --> 2.7182817182817183

;;
;; Looks pretty good!
;;

;;
;; Let's implement the same "test" procedure as in Exercise 1.37, 
;; to see how many iterations we have to carry out to get an 
;; approximation that is accurate to 4 decimal places:
;;
(define (test)
  (define tolerance 0.0001)  ;; 4 digits of accuracy
  (define e 2.718281828)
  (define target e)
  (define (test-iter k)
    (let ((value (+ 2 (cont-frac n d k))))
      (if (< (abs (- value target)) tolerance)
	  k
	  (test-iter (+ k 1)))))
  (test-iter 1))

(test)
;; --> 7

;;
;; That is, 7 iterations are required to get an approximation to e that
;; is accurate to 4 decimal places.
;;
(define e 2.718281828)

(+ 2 (cont-frac n d 7))
;; --> 2.7183098591549295

(abs (- e (+ 2 (cont-frac n d 7))))
;; --> 2.803115e-5