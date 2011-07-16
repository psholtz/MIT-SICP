(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum-integers a b)
  (accumulate + 0 identity a inc b))
(define (sum-cubes a b)
  (accumulate + 0 cube a inc b))
(define (factorial n)
  (accumulate * 1 identity 1 inc n))


;; this is iterative version
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
	result
	(accumulate-iter (next a) (combiner (term a) result))))
  (accumulate-iter a null-value))

	
  (if (> a b)
      null-value
      (accumulate-iter 