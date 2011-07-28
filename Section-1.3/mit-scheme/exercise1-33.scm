;;
;; Exercise 1.33
;;
;; You can obtain an even more general version of "accumulate" (exercise 1.32) by introducing the notion 
;; of a filter on the terms to be combined. That is, combine only those terms derived from values in the 
;; range that satisfy a specified condition. The resulting "filtered-accumulate" abstraction takes the 
;; same arguments as accumulate, together with an additional predicate of one argument that specifies
;; the filter. Write "filtered-accumulate" as a procedure. Show how to express the following using 
;; "filtered-accumulate":
;;
;; (a) the sum of the squares of the prime numbers in the interval a to b.
;;
;; (b) the product of all the positive integers less than n that are relatively prime to n.
;;

;;
;; First, define the "filtered-accumulate" procedure:
;;
(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (combiner (if (predicate a)
		    (term a)
		    null-value)
		(filtered-accumulate combiner null-value term (next a) next b predicate))))

;;
;; Define the prime predicate (take it from Exercise 1.28, which is the fastest prime testing procedure we developed):
;;
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
  (cond ((= n 1) #f)
	((= n 2) #t)
	((= n 3) #t)
	((= n 4) #f)
	((= n 5) #t)
	(else
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))

;;
;; Define other supporting procedures:
;;
(define (inc n) (+ n 1))
(define (square n) (* n n))

;; do the "prime" test
(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

;;
;; RUN SOME TESTS OF SUM OF SQUARES OF PRIMES
;;

;; 
;; ==>
;; 

;; define the "gcd" test
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(define (prime-product n)
  (filtered-accumulate * 1 identity a inc b relatively-prime?))

;;
;; RUN SOME TESTS ON RELATIVELY PRIME NUMBERS
;;

;;
;; ==>
;;

