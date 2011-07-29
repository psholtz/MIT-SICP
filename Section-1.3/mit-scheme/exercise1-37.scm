;;
;; Exercise 1.37
;;
;; [xx]
;;

;; 
;; Recursive Definition:
;;
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (cond ((= i k) (/ (n i) (d i)))
	  (else
	   (/ (n i) (+ (d i) (cont-frac-iter (+ i 1)))))))
  (cont-frac-iter 1))

;;
;; Configure numerator and denominator to always return 1.0, as in "phi" example:
;;
(define n (lambda (x) 1.0))
(define d (lambda (x) 1.0))

;; 
;; The following unit tests all assert to true:
;;
(= (/ 1.0 1.0) (cont-frac n d 1))
(= (/ 1.0 2.0) (cont-frac n d 2))
(= (/ 2.0 3.0) (cont-frac n d 3))
(= (/ 3.0 5.0) (cont-frac n d 4))
(= (/ 5.0 8.0) (cont-frac n d 5))
(= (/ 8.0 13.0) (cont-frac n d 6))
(= (/ 13.0 21.0) (cont-frac n d 7))
(= (/ 21.0 34.0) (cont-frac n d 8))
(= (/ 34.0 55.0) (cont-frac n d 9))
(= (/ 55.0 89.0) (cont-frac n d 10))

;;
;; Note the Fibonacci sequence generated in the numerator-to-denominator ratios.
;;

;;
;; Iterative:
;;
(define (cont-frac n d k)
  (define (term i t)
    (/ (n i) (+ (d i) t)))
  (define (cont-frac-iter i t)
    (cond ((= i 1) (term i t))
	  (else
	   (cont-frac-iter (- i 1) (term i t)))))
  (cont-frac-iter k 0))

    