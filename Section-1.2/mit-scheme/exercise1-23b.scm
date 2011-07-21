;;
;; Exercise 1.23b
;;
;; The optimization made in Exercise 1.23 resulted in a substantial increase in running
;; time (~1.6x to ~1.8x), but was not quite the 2x performance increase that we had 
;; anticipated.
;;
;; Let's see if we can make even further optimizations and come closer to the 2x benchmark.
;;

;;
;; First let's define the code that allows us to check for primes.
;; 
;; This code includes optimizations to "smallest-divisor", "next" and "find-divisor" procedures.
;;
(define (smallest-divisor n)
  (if (divides? 2 n)
      2
      (find-divisor n 3)))

(define (next n)
  (+ n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;
;; Let's run some unit tests, to make sure it works:
;;
(prime? 3)
;; --> #t 

(prime? 4)
;; --> #f

(prime? 5)
;; --> #t

(prime? 6)
;; --> #f

(prime? 7)
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
  (cond ((prime? n)
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
;; --> 1013 (0)
;; --> 1019 (0)
;; --> 1021 (0)
;; --> 1031 (0)
;; --> 1033 (0)
;; --> 1039 (0)
;; --> 1049 (0)

(search-for-primes 10000 10050)

;; --> 10007 (0)
;; --> 10009 (0)
;; --> 10037 (0)
;; --> 10039 (0)

(search-for-primes 100000 100050)

;; --> 100003 (0)
;; --> 100019 (0)
;; --> 100043 (0)
;; --> 100049 (1)

(search-for-primes 1000000 1000050)

;; --> 1000003 (2)
;; --> 1000033 (1)
;; --> 1000037 (1)
;; --> 1000039 (2)

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

;; --> 1009 (0)
;; --> 1013 (0)
;; --> 1019 (0)

(search-for-n-primes 10000 3)

;; --> 10007 (0)
;; --> 10009 (0)
;; --> 10037 (0)

(search-for-n-primes 100000 3)

;; --> 100003 (0)
;; --> 100019 (0)
;; --> 100043 (0) 

(search-for-n-primes 1000000 3)

;; --> 1000003 (1)
;; --> 1000033 (1)
;; --> 1000037 (2)

;;
;; These results do seem to be faster than even the code presented in part "a" of Exercise 1-23. 
;; 
;; More comprehensive analysis can be found in the corresponding .md file.
;;

;;
;; The nine primes analyzed in Exercise 1.22:
;;
(define point 1000000000)
(timed-prime-test (+ point 7))
;; --> 51 milliseconds

(timed-prime-test (+ point 9))
;; --> 51 milliseconds

(timed-prime-test (+ point 21))
;; --> 51 milliseconds 

(define point 10000000000)
(timed-prime-test (+ point 19))
;; --> 166 milliseconds

(timed-prime-test (+ point 33))
;; --> 190 milliseconds 

(timed-prime-test (+ point 61))
;; --> 190 milliseconds

(define point 100000000000)
(timed-prime-test (+ point 3))
;; --> 556 milliseconds

(timed-prime-test (+ point 19))
;; --> 559 milliseconds 

(timed-prime-test (+ point 57))
;; --> 557 milliseconds