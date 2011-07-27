
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


(define phi (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))


(define x (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))

      