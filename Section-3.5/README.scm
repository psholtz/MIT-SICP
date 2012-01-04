
;; ++++++++++++++++++ 
;; PRIME TESTING CODE
;; ++++++++++++++++++ 
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (prime? n)
  (define (get-random-a)
    (+ 2 (random (- n 4))))
  (define (test a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 2) #t)
	((= n 3) #t)
	((= n 4) #f)
	((= n 5) #t)
	(else
	 (and (test (- a 1))
	      (test (- a 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))