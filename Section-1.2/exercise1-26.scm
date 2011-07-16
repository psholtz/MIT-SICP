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
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
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

;;
;; Let's run some unit tests, to make sure it works:
;;
(define n 100)      ;; let's run the test 100 times

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
;; We'll change this somewhat from the procedures presented in the text
;; in that our procedure will only print a number (and corresponding time)
;; if it's prime.
;;
;; On MIT Scheme, the (runtime) procedure is given by (real-time-clock), and
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
  (cond ((fermat-test n)
	 (report-prime n (- (real-time-clock) start-time))
	 #t)
	(else #f)))

;;
;; Modify procedure slightly, form what is defined in the text, so that 
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
;; --> 1009 (4)
;; --> 1013 (4)
;; --> 1017 (4)
;; --> 1019 (4)
;; --> 1021 (4)
;; --> 1031 (7)
;; --> 1033 (6)
;; --> 1039 (5)
;; --> 1049 (6)

(search-for-primes 10000 10050)
;; --> 10007 (54)
;; --> 10009 (53)
;; --> 10037 (58)

(search-for-primes 100000 100050)
;; --> 100003 (542)
;; --> 100019 (563) 
;; --> 100054 (540)
;; --> 100049 (557)

(search-for-primes 1000000 1000050)
;; *** RUNS TOO SLOW TO EVALUATE ***

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
  (if (even? n)
      (search (+ a 1) 0)
      (search a 0)))

;;
;; Run the same use cases.
;;
(search-for-n-primes 1000 3)
;; --> 1009 (4)
;; --> 1013 (4)
;; --> 1017 (4)

(search-for-n-primes 10000 3)
;; --> 10007 (54)
;; --> 10009 (53)
;; --> 10037 (55)

(search-for-n-primes 100000 3)
;; --> 100003 (581)
;; --> 100019 (579)
;; --> 100054 (531)

(search-for-n-primes 1000000 3)
;; *** RUNS TOO SLOW TO EVALUATE *** 

;;
;; The nine rimes analyzed in Exercise 1.22:
;;

;;
;; The nine primes from Exercise 1.22, which are defined by:
;;
;; (define point 1000000000)
;; (define p1 (+ point 7))
;; (define p2 (+ point 9))
;; (define p3 (+ point 21))
;; 
;; (define point (* 10 point))
;; (define q1 (+ point 19))
;; (define q2 (+ point 33))
;; (define q3 (+ point 61))
;;
;; (define point (* 10 point))
;; (define r1 (+ point 3))
;; (define r2 (+ point 19))
;; (define r3 (+ point 57))
;;
;; *** ALL THESE PRIMES RUN TOO SLOW TO EVALUATE ***

;; 
;; Add procedures for calculating the "average" and "standard deviation"
;; of a set of numbers. Used (in this context) for calculating the distribution
;; in timing when finding large primes.
;;

;;
;; STATISTICS PROCEDURES
;;

;; avg : list -> float
;; average value of a list of numbers
(define (avg a)
  (define (avg-iter b t i)
    (cond ((> (length b) 0)
	   (avg-iter (cdr b) (+ (car b) t) (+ i 1)))
	  (else
	   (/ t (length a)))))
  (avg-iter a 0.0 0.0))

;;
;; Run some unit tests
;;
(avg (list 0))
;; --> 0.0

(avg (list 1))
;; --> 1.0

(avg (list 2))
;; --> 2.0

(avg (list 0 1))
;; --> 0.5

(avg (list 1 2))
;; --> 1.5 

(avg (list 0 2))
;; --> 1.0

(avg (list 0 1 2))
;; --> 1.0

;; std : list -> float
;; standard devation of a list of numbers
(define (std d)
  (let ((a (avg d)))
    (define (std-iter b t)
      (if (> (length b) 0)
	  (std-iter (cdr b) (+ (square (- (car b) a)) t))
	  (sqrt (/ t (- (length d) 1)))))
    
    ;; length 1 will trigger division by 0
    (if (= (length d) 1)
	0.0
	(std-iter d 0.0))))

;;
;; Run some unit tests
;;
(std (list 0))
;; --> 0.0

(std (list 1))
;; --> 0.0

(std (list 2))
;; --> 0.0

(std (list 0 1))
;; --> 0.7071

(std (list 0 2))
;; --> 1.4142

(std (list 0 1 2))
;; --> 1.0

(std (list 4 2 5 8 6))
;; --> 2.23607

;; gather-statistics : integer -> float
;; determine number of milliseconds to determine whether integer is prime
(define (gather-statistics number)
  (define times-to-run-test 10)
  (define (run start-time)
    (cond ((fast-prime? number times-to-run-test) (- (real-time-clock) start-time))
	  (else -1)))
  (run (real-time-clock)))

;;
;; Run some unit tests
;;
(gather-statistics 10007)
;; --> 539

(gather-statistics 10009)
;; --> 574

(gather-statistics 10037) 
;; --> 533

;; gather-n-statistics : integer , integer, bool -> list
;; generate a list of floats, signifying the time it takes to determine
;; whether the "number" integer is prime. Bool indicates whether to write
;; the results to console in real-time or not.
(define (gather-n-statistics number times console)
  (define (gather-n-statistics-iter i a)
    (let ((value (gather-statistics number)))
      (if (> value -1)
	  (cond ((< i times)
		 (cond (console
			(display value)
			(if (< i (- times 1))
			    (display " "))))
		 (gather-n-statistics-iter (+ i 1) (cons value a)))
		(else a))
	  #f)))
  (gather-n-statistics-iter 0 '()))

;;
;; Run some unit tests
;; 
(gather-n-statistics 10007 10 #t)
;; --> 557 604 653 624 700 629 629 578 635 596

(gather-n-statistics 10009 10 #t)
;; --> 556 595 531 607 595 595 565 545 616 546

(gather-n-statistics 10037 10 #t)
;; --> 553 607 570 620 571 581 547 577 567 618

;; statistics : integer, integer -> void
;; Largely "ui" oriented procedure. "number" indicates
;; the number to test for primality, while "times"
;; indicates the number of samples to take in the sample set.
(define (statistics number times)

  (define (display-statisitics)
    (newline)
    (display "Statistics for the prime ")
    (display number)
    (newline)
    (display "(")
    (define value (gather-n-statistics number times #t))
    (display ")")
    (newline)
    (display "Mean: ")
    (define a (avg value))
    (display a)
    (display " milliseconds")
    (newline)
    (define s (std value))
    (display "Std Dev: ")
    (display s)
    (newline))

  (define (display-error)
    (newline)
    (display " is not a prime")
    (newline))

  (define times-to-run-test 10)
  (cond ((fast-prime? number times-to-run-test) (display-statistics))
	(else (display-error))))

;;
;; Run some unit tests
;;
(statistics 10007 10)
;; --> (578 595 572 614 622 598 596 631 552 595)
;; --> Mean: 595.3 milliseconds
;; --> Std Dev: 23.7 

(statistics 10009 10)
;; --> (541 595 524 565 628 628 608 585 594 553)
;; --> Mean: 582.1 milliseconds
;; --> Std Dev: 35.6

(statistics 10037 10)
;; --> (585 585 578 594 612 548 634 585 590 553)
;; --> Mean: 586.4 milliseconds
;; --> Std Dev: 25.1

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
(remainder (* 5 (expmod 5 4 3)) 3)
(remainder (* 5 (remainder (* (expmod 5 2 3) (expmod 5 2 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder (* (expmod 5 1 3) (expmod 5 1 3)) 3) 
			      (remainder (* (expmod 5 1 3) (expmod 5 1 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder (* (remainder (* 5 (expmod 5 0 3)) 3) (remainder (* 5 (expmod 5 0 3)) 3)) 3)
			      (remainder (* (remainder (* 5 (expmod 5 0 3)) 3) (remainder (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder (* (remainder (* 5 1) 3) (remainder (* 5 1) 3)) 3)
			      (remainder (* (remainder (* 5 1) 3) (remainder (* 5 1) 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder (* (remainder 5 3) (remainder 5 3)) 3)
			      (remainder (* (remainder 5 3) (remainder 5 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder (* 2 2) 3)
			      (remainder (* 2 2) 3)) 3)) 3)
(remainder (* 5 (remainder (* (remainder 4 3) (remainder  4 3)) 3)) 3)
(remainder (* 5 (remainder (* 1 1) 3)) 3)
(remainder (* 5 (remainder 1 3)) 3)
(remainder (* 5 1) 3)
(remainder 5 3)
2

;;
;; In this computation, the "expmod" procedure is invoked 12 times.
;;

;; 
;; Now let's expand this expression using the "fast" computational model:
;;
(expmod 5 5 3)
(remainder (* 5 (expmod 5 4 3)) 3)
(remainder (* 5 (remainder (square (expmod 5 2 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder (square (expmod 5 1 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder (square (remainder (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder (square (remainder (* 5 1) 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder (square (remainder 5 3)) 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder (square 2) 3)) 3)) 3)
(remainder (* 5 (remainder (square (remainder 4 3)) 3)) 3)
(remainder (* 5 (remainder (square 1) 3)) 3)
(remainder (* 5 (remainder 1 3)) 3)
(remainder (* 5 1) 3)
(remainder 5 3)
2

;; 
;; In this computaiton, the "expmod" procedure is invoked 5 times. 
;;
;; Clearly this second procedure will execute much more rapidly.
;;