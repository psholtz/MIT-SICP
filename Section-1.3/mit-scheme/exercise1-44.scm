
(define (smooth f)
  (define dx 0.001)
  (define (average a b c)
    (/ (+ a b c) 3.0))
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))
