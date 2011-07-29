;;
;; Exericse 1.38
;;
(define (d i)
  (let ((r (remainder (- i 2) 3))
	(n (floor (/ i 3))))
    (cond ((= r 0) (* (+ n 1.0) 2.0))
	  (else 1.0))))

(define (n i) 1.0)

(cont-frac n d 10)
