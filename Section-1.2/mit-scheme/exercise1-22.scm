;;
;; Exercise 1.22
;;
;; Most Lisp implementations include a primitive called "runtime" that returns an 
;; integer that specifies the amount of time the system has been running (measured, 
;; for example, in microseconds). The following "timed-prime-test" procedure, when 
;; called with an integer n, prints n and checks to see if n is prime. If n is 
;; prime, the procedure prints three asterisks follows by the amount of time used 
;; in performing this test.
;;
;; Using this procedure, write a procedure "search-for-primes" that checks the 
;; primality of consecutive odd integers in a specified range. Use your procedure 
;; to find the smallest primes larger than 1000; larger than 10,000; larger than 100,000;
;; larger than 1,000,000. Note the time needed to test each prime. Since the testing
;; algorithm has order of growth O(sqrt(n)), you should expect that testing for primes
;; around 10,000 should take about sqrt(10) times as long as testing for primes around 
;; 1,000. Do your timing data bear this out? How well do the data for 100,000 and 
;; 1,000,000 support the sqrt(n) prediction? Is your result compatible with the notion
;; that programs on your machine run in time proportional to the number of steps 
;; required for the computation.
;;

;;
;; First let's define the code that allows us to check for primes:
;;
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

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

;; --> 100003 (1)
;; --> 100019 (1)
;; --> 100043 (1)
;; --> 100049 (1)

(search-for-primes 1000000 1000050)

;; --> 1000003 (2)
;; --> 1000033 (3)
;; --> 1000037 (2)
;; --> 1000039 (3)

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

;; --> 100003 (1)
;; --> 100019 (1)
;; --> 100043 (1) 

(search-for-n-primes 1000000 3)

;; --> 1000003 (2)
;; --> 1000033 (3)
;; --> 1000037 (2)