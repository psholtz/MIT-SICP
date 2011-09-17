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
(defun filtered-accumulate (combiner null-value term a next b predicate)
  (if (> a b)
      null-value
    (funcall combiner (if (funcall predicate a)
			  (funcall term a)
			null-value)
	     (filtered-accumulate combiner null-value term (funcall next a) next b predicate))))

;;
;; Define the prime predicate (take it from Exercise 1.28, which is the fastest prime testing procedure we developed):
;;
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (square (expmod base (/ exp 2) m)) m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

(defun prime? (n)
  (defun get-random-a ()
    (+ 2 (random (- n 4))))
  (defun test (a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 1) '())
	((= n 2) t)
	((= n 3) t)
	((= n 4) '())
	((= n 5) t)
	(t
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))
;;
;; Define other supporting procedures:
;;
(defun inc (n) (+ n 1))
(defun square (n) (* n n))
(defun even? (n) (= (% n 2) 0))

;;
;; Do the "prime" test
(defun sum-of-prime-squares (a b)
  (filtered-accumulate #'+ 0 #'square a #'inc b #'prime?))

;;
;; RUN SOME TESTS OF SUM OF SQUARES OF PRIMES
;;
(sum-of-prime-squares 1 1)
;; ==> 0

(sum-of-prime-squares 1 2)
;; ==> 4

(sum-of-prime-squares 1 3)
;; ==> 13

(sum-of-prime-squares 1 4)
;; ==> 13

(sum-of-prime-squares 1 5)
;; ==> 38

;;
;; We know that 1000999 is prime
;;
(setq test-value 1000999)
(= (square test-value) (sum-of-prime-squares test-value (+ test-value 1)))
;; ==> t

;; define the "gcd" test
(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (% a b))))

(defun relatively-prime? (a b)
  (= (gcd a b) 1))

(defun identity (x) x)

(defun prime-product (n)
  (defun relatively-prime-to-n? (a)
    (relatively-prime? a n))
  (filtered-accumulate #'* 1 #'identity 1 #'inc n #'relatively-prime-to-n?))

;;
;; Let's run through some unit tests:
;;
(prime-product 3)
;; ==> 2

;;
;; All integers greater than 1 are relatively prime to 1.
;; 3 is r.p. to 2 as well.
;;
;; Hence, (= (prime-product 3) 2) as expected.
;;

(prime-product 4)
;; ==> 3 

;;
;; 4 is r.p. to 1 and 3.
;; (= (prime-product 4) (* 1 3))
;;

(prime-product 5)
;; ==> 24

;;
;; 5 is r.p. to 1, 2, 3 and 4.
;; (= (prime-product 5) (* 1 2 3 4))
;;

(prime-product 6)
;; ==> 5 

;;
;; 6 is r.p. is 1 and 5.
;; (= (prime-product 6) (* 1 5))
;;

(prime-product 7)
;; ==> 720

;;
;; 7 is r.p. to 1, 2, 3, 4, 5, 6
;; (= (prime-product 7) (* 1 2 3 4 5 6))
;;

(prime-product 8)
;; ==> 105

;;
;; 8 is r.p. to 1, 3, 5, 7
;; (= (prime-product 8) (* 1 3 5 7))
;; 

(prime-product 9)
;; ==> 2240

;;
;; 9 is r.p. is 2, 4, 5, 7, 8 
;; (= (prime-product 9) (* 1 2 4 5 7 8))
;;

(prime-product 10)
;; ==> 189

;;
;; 10 is r.p. to 3, 7, 9
;; (= (prime-product 10) (* 1 3 7 9)
;;

;;
;; It's interesting to note that if n is prime, our prime product routine returns (n-1)! 
;;
;; Vide 3, 5 and 7 as examples.
;;

;; 
;; One thing we would expect is for (prime-product p) to be equal to (factorial (- p 1)), 
;; since p will be relatively prime to all positive integers less than p. 
;; 
;; Let's define a factorial procedure to test this for some primes.
;;
(defun factorial (n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t
	 (* n (factorial (- n 1))))))

(= 1 (prime-product 2))
;; ==> t

(= 2 (prime-product 3))
;; ==> t

(= (factorial 4) (prime-product 5))
;; ==> t

(= (factorial 6) (prime-product 7))
;; ==> t

(= (factorial 10) (prime-product 11))
;; ==> t

(= (factorial 12) (prime-product 13))
;; ==> t

;;
;; Let's test some smaller composite numbers.
;;

;; 4 is relatively prime to 3
(= 3 (prime-product 4))
;; --> t

;; 6 is relatively prime to 5
(= 5 (prime-product 6))
;; --> t

;; 8 is relatively prime to 3, 5 and 7
(= (* 3 5 7) (prime-product 8))
;; --> t

;; 9 is relatively prime to 2, 4, 5, 7 and 8
(= (* 2 4 5 7 8) (prime-product 9))
;; --> t

;; 10 is relatively prime to 3, 7 and 9
(= (* 3 7 9) (prime-product 10))
;; --> t