;;
;; Exercise 1.23
;;
;; The "smallest-divisor" procedure shown at the start of this section does lots of
;; needless testing: After it checks to see if the number is divisible by 2, there 
;; is no point in checking to see if it is divisible by any larger even numbers. This 
;; suggests that the values used for "test-divisor" should not be 2,3,4,5,6,... but 
;; rather 2,3,5,7,9... To implement this change, define a procedure "next" that returns
;; 3 if its input is equal to 2, and otherwise returns its input plus 2. Modify the 
;; "smallest-divisor" procedure to use "(next test-divisor)" instead of "(+ test-divisor 1)". 
;; With "timed-prime-test" incorporaitng this modified version of "smallest-divisor", run 
;; the test for each of the 12 primes found in exercise 1.22. Since this modification 
;; halves the number of test steps, you should expect it to run about twice as fast. 
;; Is this expectation confirmed? If not, what is the observed ratio of the speeds of 
;; the two algorithms, and how do you explain the fact that it is different from 2?
;;

;;
;; As before, let's first define the code that allows us to check for primes.
;;
;; The major difference between this code and that in Exercise 1.22 is
;; that here we have defined a new procedure "next", which should cut 
;; down the time needed to test a number for primality roughly by half.
;;
;; The statistics gathered at the end of this document, and discussed 
;; in greater depth in the corresponding .md file, give a more detailed
;; analysis of the performance improvement resulting from this code modification.
;;
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (cond ((= n 2) 3)
	(else (+ n 2))))

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

;; --> 100003 (1)
;; --> 100019 (1)
;; --> 100043 (1)
;; --> 100049 (0)

(search-for-primes 1000000 1000050)

;; --> 1000003 (2)
;; --> 1000033 (1)
;; --> 1000037 (2)
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

;; --> 1000003 (2)
;; --> 1000033 (1)
;; --> 1000037 (2)

;;
;; These results do seem "faster" than those obtained in the previous version of prime?
;; 
;; More comprehensive analysis can be found in the corresponding .md file.
;;

;;
;; The nine primes analyzed in Exercise 1.22:
;;
(define point 1000000000)
(timed-prime-test (+ point 7))
;; --> 58 milliseconds

(timed-prime-test (+ point 9))
;; --> 58 milliseconds

(timed-prime-test (+ point 21))
;; --> 58 milliseconds 

(define point 10000000000)
(timed-prime-test (+ point 19))
;; --> 211 milliseconds

(timed-prime-test (+ point 33))
;; --> 210 milliseconds 

(timed-prime-test (+ point 61))
;; --> 187 milliseconds

(define point 100000000000)
(timed-prime-test (+ point 3))
;; --> 620 milliseconds

(timed-prime-test (+ point 19))
;; --> 622 milliseconds 

(timed-prime-test (+ point 57))
;; --> 625 milliseconds