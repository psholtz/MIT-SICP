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

(sum-of-prime-squares 1 1)
;; --> 0

(sum-of-prime-squares 1 2)
;; --> 4 

(sum-of-prime-squares 1 3)
;; --> 13

(sum-of-prime-squares 1 4)
;; --> 13

(sum-of-prime-squares 1 5)
;; --> 38

;; 
;; We know that 1000999 is prime..
;;
(define test-value 1000999)
(= (square test-value) (sum-of-prime-squares test-value (+ test-value 1)))
;; --> #t

;; define the "gcd" test
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(define (identity x) x)

(define (prime-product n)
  (define (relatively-prime-to-n? a)
    (relatively-prime? a n))
  (filtered-accumulate * 1 identity 1 inc n relatively-prime-to-n?))

;;
;; RUN SOME TESTS ON RELATIVELY PRIME NUMBERS
;;

;; 
;; One thing we would expect is for (prime-product p) to be equal to (factorial (- p 1)), 
;; since p will be relatively prime to all positive integers less than p. 
;; 
;; Let's define a factorial procedure to test this for some primes.
;;
(define (factorial n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else
	 (* n (factorial (- n 1))))))

(= 1 (prime-product 2))
;; --> #t

(= 2 (prime-product 3))
;; --> #t

(= (factorial 4) (prime-product 5))
;; --> #t 

(= (factorial 6) (prime-product 7))
;; --> #t 

(= (factorial 10) (prime-product 11))
;; --> #t

(= (factorial 12) (prime-product 13))
;; --> #t

(= (factorial 1008) (prime-product 1009))
;; --> #t

;;
;; Let's test some smaller composite numbers.
;;

;; 4 is relatively prime to 3
(= 3 (prime-product 4))
;; --> #t

;; 6 is relatively prime to 5
(= 5 (prime-product 6))
;; --> #t

;; 8 is relatively prime to 3, 5 and 7
(= (* 3 5 7) (prime-product 8))
;; --> #t

;; 9 is relatively prime to 2, 4, 5, 7 and 8
(= (* 2 4 5 7 8) (prime-product 9))
;; --> #t

;; 10 is relatively prime to 3, 7 and 9
(= (* 3 7 9) (prime-product 10))
;; --> #t



