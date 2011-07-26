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