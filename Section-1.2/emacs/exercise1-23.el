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
;; Increase the buffers, so we can compute large primes.
;;
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;;
;; As before, let's first define the code that allows us to check for primes.
;;
;; The major difference between this code and that in Exercise 1.22 is
;; that here we have defined a new procedure "next", which should cut 
;; down the time needed to test a number for primality roughly by half.
;;
(defun smallest-divisor (n)
  (find-divisor n 2))

(defun next (n)
  (cond ((= n 2) 3)
	(t
	 (+ n 2))))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(t
	 (find-divisor n (next test-divisor)))))

(defun divides? (a b)
  (= (% b a) 0))

(defun square (x) (* x x))

(defun even? (n) (= (% n 2) 0))

(defun prime? (n)
  (= n (smallest-divisor n)))

;;
;; Let's run some unit tests, to make sure it works:
;;
(prime? 3)
;; ==> t

(prime? 4)
;; ==> nil

(prime? 5)
;; ==> t

(prime? 6)
;; ==> nil

(prime? 7)
;; ==> t

;;
;; Next, define the procedures for running timed tests.
;; 
;; We'll change this somewhat from the procedures presented in the text, 
;; in that our procedure will only print a number (and corresponding time)
;; if it's prime. 
;;
;; Emacs Lisp does not have the "real-time-clock" procedure of Scheme. 
;;
;; However, it does have "current-time", which returns a list of three
;; elements: (high low microseconds). "High" and "low" combine to give
;; the number of seconds since Jan 1, 1970, while microseconds is the 
;; time since the current second. We will form our procedure to return 
;; low + microseconds/10^6, to get (theoretically) microsecond accuracy
;; for our time measurements.
;;
(defun real-time-clock ()
  (let ((q (current-time)))
    (+ (car (cdr q))
       (/ (car (cdr (cdr q))) 1000000.0))))

;;
;; Next, define the procedures for running timed tests.
;;
;; We'll change this somewhat from the procedures presented in the text,
;; in that our procedure will only print a number (and corresponding time)
;; if it's a prime.
;;
(defun timed-prime-test (n)
  (start-prime-test n (real-time-clock)))

;;
;; Use this definition of start-prime-test, which returns "true" or "false"
;; depending on whether the test candidate is prime, so that we can more 
;; easily support the "search-for-n-primes" procedure defined below.
;;
(defun start-prime-test (n start-time)
  (cond ((prime? n)
	 (report-prime n (- (real-time-clock) start-time))
	 t)
	(t nil)))

;;
;; Modify procedure slightly, from what is defined in the text, so that 
;; we only print the prime numbers (i.e., non-primes are suppressed).
;;
(defun report-prime (n elapsed-time)
  (newline)
  (princ n)
  (princ " (")
  (princ elapsed-time)
  (princ ")")
  (newline)
  nil)

;;
;; Finally, let's define the "search-for-primes" procedure.
;;
;; The procedure will take two integers, a and b, and for each prime
;; inbetween the two integers (inclusive) it will print the prime out
;; and display the time required to calculate that it was a prime.
;;
(defun search-for-primes (a b)
  (defun search (n)
    (cond ((<= n b) (timed-prime-test n)))
    (cond ((< n b) (search (+ n 2)))))
  (if (even? a)
      (search (+ a 1))
    (search a)))

;;
;; Run use cases..
;;
(search-for-primes 1000 1050)

;; ==> 1009 (4.7e-05)
;; ==> 1013 (4.2e-05)
;; ==> 1019 (4.2e-05)
;; ==> 1021 (4.4e-05)
;; ==> 1031 (3.5e-05)
;; ==> 1033 (3.6e-05)
;; ==> 1039 (3.5e-05)
;; ==> 1049 (3.5e-05)

(search-for-primes 10000 10050)

;; ==> 10007 (0.000122)
;; ==> 10009 (0.000123)
;; ==> 10037 (0.000122)
;; ==> 10039 (0.000113)

(search-for-primes 100000 100050)

;; ==> 100003 (0.000441)
;; ==> 100019 (0.000330)
;; ==> 100043 (0.000318)
;; ==> 100049 (0.000288)

(search-for-primes 1000000 1000050)

;; ==> emacs cannot recurse this far.

;;
;; Now define one additional procedure, which starts at a number a
;; and finds the next n prime numbers (this is, technically, what 
;; Exercise 1.22 asks us to do).
;;
(defun search-for-n-primes (a n)
  (defun search (j c)
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

;; ==> 1009 (4.2e-05)
;; ==> 1013 (4.1e-05)
;; ==> 1019 (4.1e-05)

(search-for-n-primes 10000 3)

;; ==> 10007 (0.000123)
;; ==> 10009 (0.000126) 
;; ==> 10037 (0.000124)

(search-for-n-primes 100000 3)

;; ==> 100003 (0.000440)
;; ==> 100019 (0.000336)
;; ==> 100043 (0.000327)

(search-for-n-primes 1000000 3)

;; ==> emacs cannot recurse this far.