;;
;; Exercise 1.39
;;
;; A continued fraction representation of the tangent function was published in 1770 by 
;; the German mathematician J.H. Lambert:
;;
;;   tan x =           x
;;            ------------------ 
;;             1 -       x^2
;;                 --------------  
;;                 3  -   x^2
;;                      ------
;;                      5 - ...
;;
;; where x is in radians. Define a procedure "(tan-cf x k)" that computes an approximation
;; to the tangent function based on Lambert's formual. k specifies the number of terms to 
;; compute, as in Exercise 1.37
;;

(define (square x) (* x x))

;;
;; Define the numerator:
;;
(define (n i x)
  (cond ((= i 1) x)
	(else (square x))))

;; 
;; Define the denominator:
;;
(define (d i)
  (- (* 2 i) 1))

;;
;; Define the tangent function according to Lambert's formula:
;;
(define (tan-cf x k)
  (define (cont-frac-iter i)
    (cond ((= i k) (/ (n i x) (d i)))
	  (else
	   (/ (n i x) (- (d i) (cont-frac-iter (+ i 1)))))))
  (cont-frac-iter 1))

;;
;; Let's run some unit tests. 
;;
;; For instance, compare the results of the iterated-fraction version 
;; of "tan", at 10 iterations, with the library-supplied version of tan
;; found in Scheme:
;;
(tan 1.0)
;; --> 1.55740772

(tan-cf 1.0 10)
;; --> 1.55740772

;;
;; They look pretty close! 
;; 
;; In fact, we can write:
;;
(= (tan 1.0) (tan-cf 1.0 10))
;; --> #t

;;
;; and further:
;;
(= (tan 2.0) (tan-cf 2.0 15))
;; --> #t