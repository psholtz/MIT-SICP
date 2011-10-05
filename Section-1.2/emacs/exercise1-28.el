;;
;; Exercise 1.28
;;
;; One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test
;; (Miller 1976, Rabin 1980). This starts from an alternate form of Fermat's Little Theorem, 
;; which states that if n is a prime number and a is any positive integer less than n, then 
;; a raised to the (n-1)st power is congruent to 1 modulo n. To test the primality of a number n
;; by the Miller-Rabin test, we pick a random number a<n and raise a to the (n-1)st power modulo
;; n using the expmod procedure. However, whenever we perform the squaring step in expmod, 
;; we check to see if we have discovered a "nontrivial square root of 1 modulo n", that is, 
;; a number not equal to 1 or n-1 whose square is equal to 1 modulo n. It is possible to prove 
;; that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible
;; to prove that if n is an odd number that is not prime, then, for at least half the numbers
;; a<n, computing a^(n-1) in this way will reveal a nontrivial square root of 1 modulo n. 
;; (This is why the Miller-Rabin test cannot be fooled). Modify the expmod procedure to signal 
;; if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test
;; with a procedure analogous to "fermat-test". Check your procedure by testing various known primes
;; and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.
;;

;;
;; Let's start by looking at the "expmod" procedure given in the text:
;;
(defun even? (n) (= (% n 2) 0))
(defun square (n) (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (square (expmod base (/ exp 2) m)) m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

;;
;; The question asks us to select a random positive number a < n and check to see whether (expmod a (- n 1) n)
;; is congruent to 1. Let's develop such a test and run a few tractable (i.e., small) numbers (both prime and 
;; composite) through the test to get a feel for how we want the procedure to operate:
;;
(defun run-test (n)
  (defun display-expmod (a)
    (princ "(expmod ")
    (princ a)
    (princ " ")
    (princ (- n 1))
    (princ " ")
    (princ n)
    (princ ") ==> ")
    (princ (expmod a (- n 1) n)))
  (defun run-test-iter (i)
    (cond ((< i n)
	   (display-expmod i)
	   (newline)
	   (run-test-iter (+ i 1)))))
  (run-test-iter 1))

;;
;; Run some unit tests for some small primes and composites:
;;
(run-test 5)
;; (expmod 1 4 5) ==> 1  
;; (expmod 2 4 5) ==> 1  
;; (expmod 3 4 5) ==> 1  
;; (expmod 4 4 5) ==> 1 

(run-test 6)
;; (expmod 1 5 6) ==> 1
;; (expmod 2 5 6) ==> 2
;; (expmod 3 5 6) ==> 3
;; (expmod 4 5 6) ==> 4 
;; (expmod 5 5 6) ==> 5

(run-test 7)
;; (expmod 1 6 7) ==> 1
;; (expmod 2 6 7) ==> 1
;; (expmod 3 6 7) ==> 1
;; (expmod 4 6 7) ==> 1
;; (expmod 5 6 7) ==> 1
;; (expmod 6 6 7) ==> 1

(run-test 8)
;; (expmod 1 7 8) ==> 1
;; (expmod 2 7 8) ==> 0
;; (expmod 3 7 8) ==> 3 
;; (expmod 4 7 8) ==> 0
;; (expmod 5 7 8) ==> 5
;; (expmod 6 7 8) ==> 0
;; (expmod 7 7 8) ==> 7 

(run-test 9)
;; (expmod 1 8 9) ==> 1
;; (expmod 2 8 9) ==> 4
;; (expmod 3 8 9) ==> 0
;; (expmod 4 8 9) ==> 7
;; (expmod 5 8 9) ==> 7 
;; (expmod 6 8 9) ==> 0
;; (expmod 7 8 9) ==> 4
;; (expmod 8 8 9) ==> 1

(run-test 10)
;; (expmod 1 9 10) ==> 1
;; (expmod 2 9 10) ==> 2
;; (expmod 3 9 10) ==> 3
;; (expmod 4 9 10) ==> 4
;; (expmod 5 9 10) ==> 5
;; (expmod 6 9 10) ==> 6
;; (expmod 7 9 10) ==> 7
;; (expmod 8 9 10) ==> 8
;; (expmod 9 9 10) ==> 9

(run-test 11)
;; (expmod 1 10 11) ==> 1
;; (expmod 2 10 11) ==> 1
;; (expmod 3 10 11) ==> 1
;; (expmod 4 10 11) ==> 1
;; (expmod 5 10 11) ==> 1
;; (expmod 6 10 11) ==> 1
;; (expmod 7 10 11) ==> 1
;; (expmod 8 10 11) ==> 1
;; (expmod 9 10 11) ==> 1
;; (expmod 10 10 11) ==> 1

;;
;; The descriptions of the Miller-Rabin test I found on the Internet differed substantially from the description
;; given here in the SICP test. Moreover, coding the procedure precisely as described in the SICP test did not 
;; (to me) appear to give a reliable algorithm for detecting primes (perhaps I was misreading something?)
;;
;; So instead, let's use these observerations to try and code our own version of the Miller-Rabin test. 
;;
;; One observation which immediately jumps out is that running (expmod 1 (- n 1 ) n) will result in 1, no matter
;; what n is (i.e., prime or composite). Indeed, this is somehwat obvious: 1 raised to any power is 1, and the 
;; remainder of 1 when divided by any integer greater than 1 is 1. Because running (expmod 1 (- n 1) n) gives
;; us no additional information, we will not use this number in our final primal testing procedure.
;;
;; Another observation which is equally obvious is that (expmod a (- n 1) n) will evaluate to 1 for all values
;; of a when n is prime. Moreover, in our composite tests (i.e., 6, 8, 9 and 10), the only value of a for which
;; the expression (expmod a (- n 1) n) evaluated to 1 were - generally speaking - 1. The one exception to this 
;; rule is (expmod 8 8 9), which evaluates to 1 but note that 8 = (- 9 1). 
;; 
;; So suppose that we exclude 1 and (- n 1), and randomly select a number "a" in the range (inclusive) between
;; 2 and (- n 2). If evaluation of (expmod a (- n 1) n) yields 1, it seems that we should be reasonably sure that 
;; we have a prime. Conversely, if (expmod a (- n 1) n) yields anything other than 1, it seems that we should be 
;; reasonaly sure that we do *not* have a prime.
;;

;; 
;; One difficulty with this approach is suggested by the composite 221 (note that (= (* 13 17) 221)):
;;
(expmod 174 220 221)
;; --> 1

(expmod 183 220 221)
;; --> 1

(expmod 200 220 221)
;; --> 1

(expmod 220 220 221)
;; --> 1

;;
;; amongst many other exceptions for this composite.
;;

;;
;; Let's design our procedure as follows:
;;
;; (a) We will define a "test" to be evaluation of (expmod a (- n 1) n) for a sample number "a". 
;; (b) We will run the test for (- n 1)
;; (c) We will run the test for (- n 2)
;; (d) We will run the test for three randomly selected integers in the range 2 to (- n 3), inclusive.
;; (e) If each of the five tests described in (b), (c) and (d) above yield 1, we can be relatively certain 
;;     that we have a prime, and will return "true"
;; (f) For the numbers 2, 3, 4 and 5 we will return prime values by hand. 
;;
(defun prime? (n)
  ;; get a random from a sample from the restricted range we've defined above
  (defun get-random-a ()
    (+ 2 (random (- n 4))))

  ;; define our "expmod" test
  (defun test (a)
    (= (expmod a (- n 1) n) 1))

  ;; perform the prime test
  (cond ((= n 2) t)
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
;; Let's test some primes and composites, and see if we get the results we expect:
;;
(prime? 2)
;; --> t

(prime? 3)
;; --> t

(prime? 4)
;; --> nil

(prime? 5)
;; --> t

(prime? 6)
;; --> nil

(prime? 7)
;; --> t

(prime? 8)
;; --> nil

(prime? 9)
;; --> nil

(prime? 10)
;; --> nil

(prime? 11)
;; --> t
 
(prime? 13)
;; --> t

(prime? 23)
;; --> t

(prime? 25)
;; --> nil

(prime? 29)
;; --> t

(prime? 31)
;; --> t

;; 
;; So far, so good.. 
;;
;; Now let's test the primes we discovered in Exercise 1.22, as well as numbers in that range that 
;; we know to be composite:
;;
(setq point1 1000000000)
(setq point2 (* 10 point1))
(setq point3 (* 10 point2))

;;
;; The following three candidates should be prime:
;;
(prime? (+ point1 7))
;; --> t

(prime? (+ point1 9))
;; --> t

(prime? (+ point 21))
;; --> t

;;
;; The following numbers should be composite:
;;
(prime? (+ point1 5))
;; --> nil

(prime? (+ point1 11))
;; --> nil

(prime? (+ point1 13))
;; --> nil

(prime? (+ point1 19))
;; --> nil

(prime? (+ point1 23))
;; --> nil

;;
;; Emacs cannot handle primes larger than about a billion (at least, not using this algorithm).
;;
;; The algorithm "fails" by indicating false negatives, for numbers that should be prime.
;;