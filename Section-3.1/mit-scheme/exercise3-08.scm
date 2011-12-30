
;; [working]

(define (g start)
  (lambda (y)
    (begin (set! start (+ start 1))
	   start)))

(define h (g 0))

(define (f x)
  (if (= (h x) 1)
      x
      0))
