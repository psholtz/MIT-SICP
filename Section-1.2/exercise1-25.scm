;;
;; Exercise 1.25
;;
;; Alyssa P. Hacker complains that we went to a lot of extra work in writing "expmod". After all, she says, 
;; since we already know how to compute exponentials, we could have simply written
;;
;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;;
;; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.
;;

;; 
;; Ironically, the method proposed here by Alyssa would result in far more computational work being expended
;; (e.g., for calculating primes) than by using the original definition of "expmod". To see why this is, 
;; consider again the definition of the "fermat-test" procedure:
;;
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; The test for primality is made at the following line:

(= (expmod a n n) a)

;; where n is the candidate prime, and a is a randomly chosen integer less than n.

;;
;; The genius of the "expmod" procedure given in the text is that the procedure is able to arrive at what
;; the correct answer is, without actually going through an calculating the value of a^n (which, in the case of 
;; large candidate primes, could be enormous).
;;

;;
;; Consider testing the number 1000999 (which is, in fact, prime) for primality using this procedure, and 
;; consider first the algorithm originally given for "expmod" in the text:
;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

;;
;; Suppose we choose our randomly sampled integer, a, to be 10,000 and thus evaluate the expression 
;; (expmod 10000 1000999 1000999). For the sake of clarity, we will make some simplifying definitions:
;;
(expmod 10000 1000999 1000999)
(remainder (* 10000 (expmod 10000 1000998 1000999)) 1000999)
(remainder (* 10000 (remainder (square (expmod 10000 500499 1000999)) 1000999)) 1000999)

;;
;; ...
;;
;; It would be tedious to continue expanding the expression in detail, but it is clear that with each
;; invocation of "expmod", the argument exp is reduced, either by 1 or by one half. Continuing on from 
;; exponent 500499, the successive arguments to "expmod" will look like:
;;

;; 500499 --> 500498
;; 500498 --> 250499
;; 250499 --> 250498
;; 250498 --> 125249
;; 125249 --> 125248
;; 125248 --> 62624
;; 62624 --> 31312 
;; 31312 --> 15656
;; 15656 --> 7828
;; 7828 --> 3914
;; 3914 --> 1957
;; 1957 --> 1956
;; 1956 --> 978
;; 978 --> 489
;; 489 --> 488
;; 488 --> 244
;; 244 --> 122
;; 122 --> 61
;; 61 --> 60
;; 60 --> 30 
;; 30 --> 15 
;; 15 --> 14 
;; 14 --> 7
;; 7 --> 6
;; 6 --> 3
;; 3 --> 2
;; 2 --> 1
;; 1 --> 0

;;
;; At which point, expansion of "expmod" terminates and the recursion unfolds back on itself. 
;;
;; So for primes of size roughly around 1 million, the expansion of "expmod" requires about 30 operations.
;;

;; 
;; But now compare this with the algorithm proposed by Alyssa. Suppose we define "expmod" as proposed by Alyssa:
;;
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;
;; And suppose that we now desire to calculate (expmod 10000 1000999 1000999) again.
;;
;; Rather than terminating in roughly 30 operations, we must instead first calculate the value of 10000^1000999, 
;; that is to say, we must first calculate a number whose size is on the order of a 1 followed by 5 million zeros(!!)
;; Once that calculation has completed, we must then take the modulus of that number relative to 1000999(!!).
;;
;; Clearly, this is a far more inefficient way to search for primes, and for primes which are even moderately charge, 
;; the procedure proposed by Alyssa will either hang or crash the interpreter completely.
;;