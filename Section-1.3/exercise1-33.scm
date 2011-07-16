
(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (combiner (if (predicate (term a))
		    (term a)
		    null-value)
		(accumulate combiner null-value term (next a) next b predicate))))

;; supporting methods for "prime test"
(define (square x) (* x x))

(define (identity x) x)


;; do the "prime" test
(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; define the "gcd" test
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(define (prime-product n)
  (filtered-accumulate * 1 identity a inc b relatively-prime?))

