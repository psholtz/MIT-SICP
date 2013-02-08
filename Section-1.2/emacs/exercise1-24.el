;;
;; Exercise 1.24
;;
;; Modify the "timed-prime-test" procedure of Exercise 1.22 to use "fast-prime?" (the Fermat method), and test 
;; each of the 12 primes you found in that exercise. Since the Fermat test has O(lg n) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1,000? Do 
;; your data bear this out? Can you explain any discrepency you find?
;;

;;
;; Increase the buffers, so we can compute large primes:
;;
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;;
;; First let's define the code that allows us to check for primes:
;;
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (square (expmod base (/ exp 2) m)) m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(t '())))

(defun even? (n) (= (% n 2) 0))

(defun square (n) (* n n))

;;
;; Let's run some unit tests, to make sure it works:
;;
(fermat-test 3)
;; ==> t

(fermat-test 4)
;; ==> nil

(fermat-test 5)
;; ==> t

(fermat-test 6)
;; ==> nil

(fermat-test 7)
;; ==> t

(defvar n 100)  ;; run the test 100 times
(fast-prime? 3 n)
;; ==> t
(fast-prime? 4 n)
;; ==> nil
(fast-prime? 5 n)
;; ==> t
(fast-prime? 6 n)
;; ==> nil
(fast-prime? 7 n)
;; ==> t

;;
;; Next, define the procedures for running timed tests.
;;
;; We'll change this somewhat from the procedures presented in the text
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
  (setq times-to-run-test 10)
  (cond ((fast-prime? n times-to-run-test)
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
;; ==> 1009 (0.00023100000004205867)
;; ==> 1013 (0.0002389999999650172)
;; ==> 1019 (0.0002550000000383079)
;; ==> 1021 (0.0002389999999650172)
;; ==> 1031 (0.00019100000002936213)
;; ==> 1033 (0.00018299999999271677)
;; ==> 1039 (0.00020600000004833419)
;; ==> 1049 (0.00019499999996241968)

(search-for-primes 10000 10050)
;; ==> 10007 (0.0004550000003291643)
;; ==> 10009 (0.00038700000004610047)
;; ==> 10037 (0.0003639999995357357)
;; ==> 10039 (0.00034299999970244244)

(search-for-primes 100000 100050)
;; ==> 100003 (0.00037599999996018596)
;; ==> 100019 (0.00039400000059686136)
;; ==> 100043 (0.0003930000002583256)
;; ==> 100049 (0.00036999999974796083)

(search-for-primes 1000000 1000050)
;; ==> 1000003 (0.000578000000132306)
;; ==> 1000033 (0.000503000000207976)
;; ==> 1000037 (0.00044999999954598024)
;; ==> 1000039 (0.0004669999998441199)

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

(search-for-n-primes 10000 3)
;; ==> 10007 (0.00044499999967229087)
;; ==> 10009 (0.0004019999996671686)
;; ==> 10037 (0.0003710000000864966)

(search-for-n-primes 100000 3)
;; ==> 100003 (0.00034499999947001925)
;; ==> 100019 (0.00032900000041991007)
;; ==> 100043 (0.000323999999636726)

(search-for-n-primes 1000000 3)
;; ==> 1000003 (0.0003909999995812541)
;; ==> 1000033 (0.00038300000051094685)
;; ==> 1000037 (0.00037199999951553764)