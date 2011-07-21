;;
;; Exercise 1.24
;;
;; SICP was written in the mid-1980s. For a circa 2010 computing device, the Fermat Method simply executes
;; too fast for primes near 1 million (or, for that matter, even near 100 billion) to be "measurable" .. 
;; that is, the method returns nearly "instanteously". The data collected in the accompanying .scm file
;; for primes near 100 billion are all in the range of 0 to 1 milliseconds, too small (fast) to determine
;; a meaningful stastistical distribution.
;;
;; Accordingly, we will "start" the evaluation at a google, that is, at 10^100.
;;
;; We will collect statistics for finding primes near a google, and for finding primes near a google squared, 
;; and see whether these results match our expectations.
;;

;;
;; Define the Fermat Test:
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

;;
;; In order to calculate a google, we will use the fast-expt procedure:
;;
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define google (fast-expt 10 100))
(define google-squared (square (fast-expt 10 100)))

;;
;; Define the procedures for our test harness:
;;
(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (define times-to-run-test 10)
  (cond ((fast-prime? n times-to-run-test)
	 (report-prime n (- (real-time-clock) start-time))
	 #t)
	(else #f)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " (")
  (display elapsed-time)
  (display ")"))

;;
;; Define the procedures for search for primes:
;;
(define (search-for-primes a b)
  (define (search n)
    (cond ((<= n b) (timed-prime-test n)))
    (cond ((< n b) (search (+ n 2)))))
  (if (even? a)
      (search (+ a 1))
      (search a)))

(define (search-for-n-primes a n)
  (define (search j c)
    (let ((next-j (+ j 2)))
      (cond ((< c n)
	     (if (timed-prime-test j)
		 (search next-j (+ c 1))
		 (search next-j c))))))
  (if (even? a)
      (search (+ a 1) 0)
      (search a0)))

;;
;; Let's look for the first three primes bigger than a google:
;;
(search-for-n-primes google 3)
;; --> (+ google 267) (54)
;; --> (+ google 949) (31)
;; --> (+ google 1243) (30)

(search-for-n-primes google-squared 3)
;; --> (+ google-squared 357) (137)
;; --> (+ google-squared 627) (138)
;; --> (+ google-squared 799) (137)

;;
;; Let's save the primes we just found:
;;
(define p1 (+ google 267))
(define p2 (+ google 949))
(define p3 (+ google 1243))

(define q1 (+ google-squared 357))
(define q2 (+ google-squared 627))
(define q3 (+ google-squared 799))

;;
;; Now define the methods we use for calculating statistics:
;;
(define (avg a)
  (define (avg-iter b t i)
    (cond ((> (length b) 0)
	   (avg-iter (cdr b) (+ (car b) t) (+ i 1)))
	  (else
	   (/ t (length a)))))
  (avg-iter a 0.0 0.0))

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

;; gather-statistics : integer -> float
;; determine number of milliseconds to determine whether integer is prime
(define (gather-statistics number)
  (define times-to-run-test 10)
  (define (run start-time)
    (cond ((fast-prime? number times-to-run-test) (- (real-time-clock) start-time))
	  (else -1)))
  (run (real-time-clock)))

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

;; ++++++++++
;; STATISTICS
;; ++++++++++ 
(define (statistics number times)
  
  (define (display-statistics)
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
    (display " milliseconds")
    (newline)
    (define s (std value))
    (display "Std Dev: ")
    (display s))

  (define (display-error)
    (newline)
    (display number)
    (display " is not a prime")
    (newline))

  (cond ((fermat-test number) (display-statistics))
	(else (display-error))))

;;
;; Run some statistical tests.
;;
;; Run tests for a GOOGLE.
;;
(statistics p1 10)
;; --> (30 30 30 57 31 30 31 32 30 30)
;; --> Mean: 33.1
;; --> Std Dev: 8.43

(statistics p2 10)
;; --> (31 55 33 31 31 30 33 30 30 31)
;; --> Mean: 33.5
;; --> Std Dev: 7.64

(statistics p3 10)
;; --> (30 30 31 33 31 31 31 33 30 55)
;; --> Mean: 33.5
;; --> Std Dev: 7.64

;;
;; Run tests for a GOOGLE-SQUARED
;;
(statistics q1 10)
;; --> (140 165 141 141 165 140 141 165 141 140)
;; --> Mean: 147.9
;; --> Std Dev: 11.8
 
(statistics q2 10)
;; --> (140 140 164 142 141 166 140 166 140 140)
;; --> Mean: 147.9
;; --> Std Dev: 12.1

(statistics q3 10)
;; --> (141 141 165 140 140 166 141 141 166 141)
;; --> Mean: 148.2
;; --> Std Dev: 12.1