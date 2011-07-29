;; 
;; Recursive:
;;
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (cond ((= i k) (/ (n i) (d i)))
	  (else
	   (/ (n i) (+ (d i) (cont-frac-iter (+ i 1)))))))
  (cont-frac-iter 1))

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

    