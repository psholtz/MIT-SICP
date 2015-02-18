;; [working]

;; LCG by Knuth
(define (rand-update x)
  (let ((a 6364136223846793005)
	(c 1442695040888963407)
	(m (expt 2 64)))
    (modulo (+ (* a x) c) m)))

(define rand 
  (let ((x 0))  ;; start seed at 0
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? m 'reset) 
	     (lambda (seed)
	       (begin (set! x seed)
		      seed)))
	    (else
	     (error "Unknown message --" m))))
    dispatch))  
