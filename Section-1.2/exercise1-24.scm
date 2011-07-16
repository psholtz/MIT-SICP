;;
;; Exercise 1.24
;;
;; Modify the "timed-prime-test" procedure of Exercise 1.22 to use "fast-prime?" (the Fermat method), and test 
;; each of the 12 primes you found in that exercise. Since the Fermat test has O(lg n) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1,000? Do 
;; your data bear this out? Can you explain any discrepency you find?
;;

;;
;; First let's define the code that allows us to check for primes:
;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (even? n) (= (remainder n 2) 0))

(define (square x) (* x x))

;;
;; Let's run some unit tests, to make sure it works:
;;
(fermat-test 3)
;; --> #t 

(fermat-test 4)
;; --> #f

(fermat-test 5)
;; --> #t

(fermat-test 6)
;; --> #f

(fermat-test 7)
;; --> #t

(define n 100) ;; run the test 100 times
(fast-prime? 3 n)
;; --> #t

(fast-prime? 4 n)
;; --> #f

(fast-prime? 5 n)
;; --> #t 

(fast-prime? 6 n)
;; --> #f

(fast-prime? 7 n)
;; --> #t 

;;
;; Next, define the procedures for running timed tests.
;; 
;; We'll change this somewhat from the procedures presented in the text, 
;; in that our procedure will only print a number (and corresponding time)
;; if it's prime. 
;;
;; On MIT Scheme, the (runtime) primitive is given by (real-time-clock), and 
;; this is the procedure we will use in our code.
;;
(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))

;;
;; Use this definition of start-prime-test, which returns "true" or "false" 
;; depending on whether the test candidate is prime, so that we can more easily 
;; support the "search-for-n-primes" procedure defined below. 
;;
(define (start-prime-test n start-time)
  (define times-to-run-test 10)
  (cond ((fast-prime? n times-to-run-test)
	 (report-prime n (- (real-time-clock) start-time))
	 #t)
	(else #f)))

;;
;; Modify procedure slightly, from what is defined in the text, so that 
;; we only print the prime numbers (i.e., non-primes are suppressed).
;;
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " (")
  (display elapsed-time)
  (display ")"))

;;
;; Finally, let's define the "search-for-primes" procedure.
;;
;; The procedure will take two integers, a and b, and for each prime
;; inbetween the two integers (inclusive) it will print the prime out 
;; and display the time required to calculate that it was a prime.
;;
(define (search-for-primes a b)
  (define (search n)
    (cond ((<= n b) (timed-prime-test n)))
    (cond ((< n b) (search (+ n 2)))))
  (if (even? a)
      (search (+ a 1))
      (search a)))

;;
;; Run use cases..
;;
(search-for-primes 1000 1050)

;; --> 1009 (0)
;; --> 1013 (1)
;; --> 1019 (0)
;; --> 1021 (1)
;; --> 1031 (0)
;; --> 1033 (1)
;; --> 1039 (0)
;; --> 1049 (0)

(search-for-primes 10000 10050)

;; --> 10007 (1)
;; --> 10009 (0)
;; --> 10037 (1)
;; --> 10039 (0)

(search-for-primes 100000 100050)

;; --> 100003 (1)
;; --> 100019 (1)
;; --> 100043 (0)
;; --> 100049 (0)

(search-for-primes 1000000 1000050)

;; --> 1000003 (1)
;; --> 1000033 (0)
;; --> 1000037 (0)
;; --> 1000039 (1)

;;
;; Now define one additional procedure, which starts at a number a 
;; and finds the next n prime numbers (this is, technically, what 
;; Exercise 1.22 asks us to do).
;;
(define (search-for-n-primes a n)
  (define (search j c)
    (let ((next-j (+ j 2)))
      (cond ((< c n)
	     (if (timed-prime-test j)
		 (search next-j (+ c 1))
		 (search next-j c))))))
  (if (even? a)
      (search (+ a 1) 0)
      (search a 0)))

;;
;; Run the same use cases.
;;
(search-for-n-primes 1000 3)

;; --> 1009 (1)
;; --> 1013 (0)
;; --> 1019 (1)

(search-for-n-primes 10000 3)

;; --> 10007 (0)
;; --> 10009 (1)
;; --> 10037 (1)

(search-for-n-primes 100000 3)

;; --> 100003 (0)
;; --> 100019 (1)
;; --> 100043 (0) 

(search-for-n-primes 1000000 3)

;; --> 1000003 (1)
;; --> 1000033 (1)
;; --> 1000037 (1)

;;
;; These results seem very substantially faster than the results obtained in the 1.22 version of prime?
;;
;; More comprehensive analysis can be found in the corresponding .md file.
;;

;; 
;; The nine primes analyzed in Exercise 1.22:
;;
(define point 1000000000)
(timed-prime-test (+ point 7))
;; --> 2 milliseconds

(timed-prime-test (+ point 9))
;; --> 1 millisecond

(timed-prime-test (+ point 21))
;; --> 1 millisecond

(define point (* 10 point))
(timed-prime-test (+ point 19))
;; --> 1 millisecond

(timed-prime-test (+ point 33))
;; --> 1 millisecond

(timed-prime-test (+ point 61))
;; --> 1 millisecond

(define point (* 10 point))
(timed-prime-test (+ point 3))
;; --> 2 milliseconds

(timed-prime-test (+ point 19))
;; --> 1 millisecond 

(timed-prime-test (+ point 57))
;; --> 1 millisecond