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
;; Increase the buffers, so we can compute large primes:
;;
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;;
;; First let's define the code that allows us to check for primes:
;;
(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(t 
	 (find-divisor n (+ test-divisor 1)))))

(defun divides? (a b)
  (= (% b a) 0))

(defun square (n) (* n n))

(defun even? (n) (= (% n 2) 0))

(defun prime? (n)
  (= n (smallest-divisor n)))

;;
;; Let's run some unit tests, to make sure it works:
;;
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
;; Run use cases...
;;
(search-for-primes 1000 1050)

;; ==> 1009 (5.90e-5)
;; ==> 1013 (4.10e-5)
;; ==> 1019 (3.80e-5)
;; ==> 1021 (3.70e-5)
;; ==> 1031 (3.70e-5)
;; ==> 1033 (3.90e-5)
;; ==> 1039 (3.80e-5)
;; ==> 1049 (4.10e-5)

(search-for-primes 10000 10050)

;; ==> 10007 (0.000156)
;; ==> 10009 (0.000137)
;; ==> 10037 (0.000148)
;; ==> 10039 (0.000121)

(search-for-primes 100000 100050)

;; ==> 100003 (0.000580)
;; ==> 100019 (0.000431)
;; ==> 100043 (0.000475)
;; ==> 100049 (0.000378)

(search-for-primes 1000000 1000050)

;; ==> emacs is not able to recurse this deeply

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
;; Run the same use cases
;;
(search-for-n-primes 1000 3)

;; ==> 1009 (3.81e-5)
;; ==> 1013 (3.70e-5)
;; ==> 1019 (3.70e-5)

(search-for-n-primes 10000 3)

;; ==> 10007 (0.000119) 
;; ==> 10009 (0.000114)
;; ==> 10037 (0.000118)

(search-for-n-primes 100000 3)

;; ==> 100003 (0.000612)
;; ==> 100019 (0.000496)
;; ==> 100043 (0.000435)

(search-for-n-primes 1000000 3)

;; emacs is not able to recurse this deeply