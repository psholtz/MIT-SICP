;;
;; Statistics 
;;
;; Collect some performance statistics, to see how this procedure compares with the other prime
;; testing procedures we developed in this section.
;;

;;
;; Define the prime-testing procedure
;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (prime? n)
  (define (get-random-a)
    (+ 2 (random (- n 4))))
  (define (test a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 2) #t)
	((= n 3) #t)
	((= n 4) #f)
	((= n 5) #t)
	(else
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))

;; avg : list -> float
;; average value for a list of numbers
(define (avg a)
  (define (avg-iter b t i)
    (cond ((> (length b) 0)
	   (avg-iter (cdr b) (+ (car b) t) (+ i 1)))
	  (else
	   (/ t (length a)))))
  (avg-iter a 0.0 0.0))

;; std : list -> float
;; standard deviation for a list of numbers
(define (std d)
  (let ((a (avg d)))
    (define (std-iter b t)
      (if (> (length b) 0)
	  (std-iter (cdr b) (+ (square (- (car b) a)) t))
	  (sqrt (/ v (- (length d) 1)))))
    (if (= (length d) 1)
	0.0
	(std-iter d 0.0))))

;; gather-statistics : integer -> float
;; determine number of milliseconds to test the primality of n
(define (gather-statistics number)
  (define (run start-time)
    (cond ((prime? number) (- (real-time-clock) start-time))
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

;; statistics : integer, integer -> void
;; Largely "ui" oriented method. "number" indicates
;; the number to test for primality, while "times"
;; indicates the number of samples to take in the sample set.
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
    (display a)
    (display " milliseconds")
    (newline)
    (define s (std value))
    (display "Std Dev: ")
    (display s)
    (newline))
  (define (display-error)
    (newline)
    (display number)
    (display " is not a prime")
    (newline))
  (cond ((prime? number) (display-statistics))
	(else (display-error))))


;;
;; Define the fast-expt procedure, so we can speak in terms of "google"
;;
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define google (fast-expt 10 100))
(define google-squared (square google))

(define billion (fast-expt 10 9))
(define ten-billion (* 10 billion))
(define hundred-billion (* 100 billion))

;;
;; Let's collect some statistics for primes near 1 billion:
;;
(statistics (+ billion 7) 10)
;; --> (1 0 1 0 1 1 0 1 1 0)
;; --> Mean: 0.6 milliseconds
;; --> Std Dev: 0.52

(statistics (+ billion 9) 10)
;; --> (0 1 0 1 1 0 1 0 1 1)
;; --> Mean: 0.6 milliseconds
;; --> Std Dev: 0.52

(statistics (+ billion 21) 10)
;; --> (0 0 1 0 1 1 0 1 0 1)
;; --> Mean: 0.5 milliseconds
;; --> Std Dev: 0.53

;; 
;; Collect statistics for primes around 10 billion:
;;
(statistics (+ ten-billion 19) 10)
;; --> (0 1 1 0 1 1 0 1 1 1)
;; --> Mean: 0.7 milliseconds
;; --> Std Dev: 0.48

(statistics (+ ten-billion 33) 10)
;; --> (0 1 1 0 1 1 0 0 1 1)
;; --> Mean: 0.6 milliseconds
;; --> Std Dev: 0.52

(statistics (+ ten-billion 61) 10)
;; --> (1 0 1 1 1 0 1 1 0 1)
;; --> Mean: 0.7 milliseconds
;; --> Std Dev: 0.48

;;
;; Collect statistics for primes around 100 billion:
;;
(statistics (+ hundred-billion 3) 10)
;; --> (1 0 0 1 1 1 0 1 1 1)
;; --> Mean: 0.7 milliseconds
;; --> Std Dev: 0.48

(statistics (+ hundred-billion 19) 10)
;; --> (1 1 1 0 1 1 1 1 0 1)
;; --> Mean: 0.8 milliseconds
;; --> Std Dev: 0.42

(statistics (+ hundred-billion 57) 10)
;; --> (1 1 1 1 1 0 1 1 1 1)
;; --> Mean: 0.9 milliseconds
;; --> Std Dev: 0.32

;;
;; Collect statistics for primes around a google:
;;
(statistics (+ google 267) 10)
;; --> (14 13 14 14 13 14 41 14 13 14)
;; --> Mean: 16.4 milliseconds
;; --> Std Dev: 8.66

(statistics (+ google 949) 10)
;; --> (13 14 14 14 13 14 14 15 14 14)
;; --> Mean: 13.9 milliseconds
;; --> Std Dev: 0.57

(statistics (+ google 1243) 10)
;; --> (14 13 14 14 14 14 13 17 14 14)
;; --> Mean: 14.1 milliseconds
;; --> Std Dev: 1.10

;;
;; Collect statistics for primes around a google-squared:
;;
(statistics (+ google-squared 357) 10)
;; --> (60 62 60 62 59 85 59 62 60 62)
;; --> Mean: 63.1 milliseconds
;; --> Std Dev: 7.85

(statistics (+ google-squared 627) 10)
;; --> (59 63 60 62 68 86 59 62 59 63)
;; --> Mean: 64.1 milliseconds
;; --> Std Dev: 8.17

(statistics (+ google-squared 799) 10)
;; --> (60 63 60 62 60 86 60 63 60 63)
;; --> Mean: 63.7 milliseconds
;; --> Std Dev: 7.96

;; 
;; Clearly, this is the "fastest" prime-testing procedure we have worked with yet, and 
;; it will serve as our "go-to" prime algorithm in the future, when we need to find large primes.
;;