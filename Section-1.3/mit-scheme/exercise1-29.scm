(define (simpson f a b n)
  (define (even? k) (= (remainder k 2) 0))
  (define (odd? k) (not (even? k)))
  (define h (/ (- b a) n))
  (define (next x) (+ x h))
  (define (term a i n)
    (cond ((= i 0) (f a))
	  ((= i n) (f a))
	  ((even? i) (* 2 (f a)))
	  ((odd? i) (* 4 (f a)))))
  (define (sum term a next b i n)
    (if (> a b)
	0
	(+ (term a i n)
	   (sum term (next a) next b (+ i 1) n))))

  ;;
  ;; Check to make sure we have even n.
  ;;
  (if (even? n)
      (* (/ h 3) (sum term a next b 0 n))
      '()))
	   