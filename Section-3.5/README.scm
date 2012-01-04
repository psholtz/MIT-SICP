;; +++++++++++ 
;; SECTION 3.1
;; +++++++++++

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

;; ++++++++++++++++++++++
;; LIST MANIPULATION CODE 
;; ++++++++++++++++++++++ 
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else 
	 (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


;;
;; Compare the two summation procedures:
;;

;; 
;; First summation procedure:
;;
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
	  ((prime? count) (iter (+ count 1) (+ count accum)))
	  (else 
	   (iter (+ count 1) accum))))
  (iter a 0))

;;
;; Second summation procedure:
;;
(define (sum-primes a b)
  (accumulate +
	      0
	      (filter prime? (enumerate-interval a b))))