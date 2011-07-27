(define (cont-fact n d k)
  (define (iter i t)
    (if (= i 0)
	t
	(iter (- i 1) (/ (n i) (+ (d i) t)))))
  (iter k 0))

(cont-fact (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   20)
