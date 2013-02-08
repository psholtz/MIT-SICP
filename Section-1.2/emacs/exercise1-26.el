;;
;; Exercise 1.26
;;
;; Louis Reasoner is having great difficulty doing exercise 1.24. This "fast-prime?" test seems to 
;; run more slowly than this "prime?" test. Louis calls his friend Eva Lu Ator over to help. When 
;; they examine Louis' code, they find that he has rewritten the expmod procedure to use an explicit
;; multiplication, rather than calling "square":
;;
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                      m))
;;          (else 
;;           (remainder (* base (expmod base (- exp 1) m))
;;                      m))))
;;
;; "I don't see what difference that could make," says Louis. "I do." says Eva. "By writing the 
;; "procedure like that you have transformed the O(log n) process into a O(n) process." Explain.
;;

;;
;; For the sake of entertainment, let's set ourselves up to run the prime number tests using the 
;; version of "expmod" given above:
;;
(defun even? (n) (= (% n 2) 0))
(defun square (n) (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (* (expmod base (/ exp 2) m)
	       (expmod base (/ exp 2) m))
	    m))
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

;;
;; Let's run some unit tests, to make sure it works:
;;
(defvar n 100)

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
  (cond ((fermat-test n)
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
;; ==> 1009 (0.0019820000015897676)
;; ==> 1013 (0.0020189999995636754)
;; ==> 1019 (0.0019709999978658743)
;; ==> 1021 (0.001967000003787689)
;; ==> 1031 (0.002812999999150634)
;; ==> 1033 (0.002759000002697576)
;; ==> 1035 (0.0028040000033797696)
;; ==> 1039 (0.002847000003384892)
;; ==> 1045 (0.0028890000030514784)
;; ==> 1049 (0.002803000003041234)

(search-for-primes 10000 10050)
;; ==> 10005 (0.02461300000140909)
;; ==> 10007 (0.024306999999680556)
;; ==> 10009 (0.024612000001070555)
;; ==> 10037 (0.024416000000201166)
;; ==> 10039 (0.02485300000262214)

(search-for-primes 100000 100050)
;; ==> 100003 (0.22030800000356976)
;; ==> 100019 (0.21976299999369076)
;; ==> 100043 (0.2180900000021211)
;; ==> 100049 (0.22086800000397488)

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

(search-for-n-primes 1000 3)
;; ==> 1009 (0.0019860000029439107)
;; ==> 1013 (0.00195499999972526)
;; ==> 1019 (0.0019460000039543957)

(search-for-n-primes 10000 3)
;; ==> 10007 (0.02529800000047544)
;; ==> 10009 (0.024672999999893364)
;; ==> 10037 (0.024954000000434462)

(search-for-n-primes 100000 3)
;; ==> 100003 (0.2195949999950244)
;; ==> 100019 (0.22093199999653734)
;; ==> 100043 (0.22337200000038138)

;; 
;; Clearly the performance of this procedure is **much** slower than that given in 
;; Exercise 1-22, and just as clearly the reason is because we are performing the 
;; recursive calculation "expmod" twice, rather than just once as in Exercise 1-22.
;; 
;; Let's expand the expression (expmod 5 5 3) using both models, to get a better sense 
;; of what's going on. 
;;
;; First we expand using the "slow" computational model:
;;
(expmod 5 5 3)
(% (* 5 (expmod 5 4 3)) 3)
(% (* 5 (% (* (expmod 5 2 3) (expmod 5 2 3)) 3)) 3)
(% (* 5 (% (* (% (* (expmod 5 1 3) (expmod 5 1 3)) 3)
	      (% (* (expmod 5 1 3) (expmod 5 1 3)) 3)) 3)) 3)
(% (* 5 (% (* (% (* (% (* 5 (expmod 5 0 3)) 3) (% (* 5 (expmod 5 0 3)) 3)) 3)
	      (% (* (% (* 5 (expmod 5 0 3)) 3) (% (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(% (* 5 (% (* (% (* (% (* 5 1) 3) (% (* 5 1) 3)) 3)
	      (% (* (% (* 5 1) 3) (% (* 5 1) 3)) 3)) 3)) 3)
(% (* 5 (% (* (% (* (% 5 3) (% 5 3)) 3)
	      (% (* (% 5 3) (% 5 3)) 3)) 3)) 3)
(% (* 5 (% (* (% (* 2 2) 3)
	      (% (* 2 2) 3)) 3)) 3)
(% (* 5 (% (* (% 4 3) (% 4 3)) 3)) 3)
(% (* 5 (% (* 1 1) 3)) 3)
(% (* 5 (% 1 3)) 3)
(% (* 5 1) 3)
(% 5 3)
2

;;
;; In this computation, the "expmod" procedure is invoked 12 times.
;;

;; 
;; Now let's expand this expression using the "fast" computational model:
;;
(expmod 5 5 3)
(% (* 5 (expmod 5 4 3)) 3)
(% (* 5 (% (square (expmod 5 2 3)) 3)) 3)
(% (* 5 (% (square (% (square (expmod 5 1 3)) 3)) 3)) 3)
(% (* 5 (% (square (% (square (% (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(% (* 5 (% (square (% (square (% (* 5 1) 3)) 3)) 3)) 3)
(% (* 5 (% (square (% (square (% 5 3)) 3)) 3)) 3)
(% (* 5 (% (square (% (square 2) 3)) 3)) 3)
(% (* 5 (% (square (% 4 3)) 3)) 3)
(% (* 5 (% (square 1) 3)) 3)
(% (* 5 (% 1 3)) 3)
(% (* 5 1) 3)
(% 5 3)
2

;; 
;; In this computaiton, the "expmod" procedure is invoked 5 times. 
;;
;; Clearly this second procedure will execute much more rapidly.
;;