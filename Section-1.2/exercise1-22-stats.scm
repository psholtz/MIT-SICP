;;
;; Code for calculating statistics.
;; 
;; Includes procedures for calculating the "average" and "standard deviation"
;; of a set of numbers. Used (in this context) for calculating the distributions
;; in timing when finding large primes.
;;

;;
;; PRIME TESTING PROCEDURES (1.22)
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
  (= (smallest-divisor n) n))

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
;; --> 0 

(avg (list 1))
;; --> 1 

(avg (list 2))
;; --> 2

(avg (list 0 1))
;; --> 0.5

(avg (list 1 2))
;; --> 1.5

(avg (list 0 2))
;; --> 1.0 

(avg (list 0 1 2))
;; --> 1.0

;; std : list -> float
;; standard deviation of a list of numbers
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
;; --> 0.707

(std (list 1 2))
;; --> 0.707

(std (list 0 2))
;; --> 1.414

(std (list 0 1 2))
;; --> 1.0

(std (list 4 2 5 8 6))
;; --> 2.24

;; gather-statistics : integer -> float
;; determine number of milliseconds to determine whether integer is prime
(define (gather-statistics number)
  (define (run start-time)
    (cond ((prime? number) (- (real-time-clock) start-time))
	  (else -1)))
  (run (real-time-clock)))

;;
;; Run some unit tests
;;
(gather-statistics 1000999)
;; --> 2

(gather-statistics 1000000007)
;; --> 95

;; gather-n-statistics : integer, integer, bool -> list
;; generate a list of floats, signifying the time is takes to determine 
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
(gather-n-statistics 1000999 10 #f)
;; --> (3 2 3 3 2 3 2 3 2 3)

(gather-n-statistics 1000000007 10 #f)
;; --> (118 94 94 93 94 119 97 99 98 97)

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
;; Run some unit tests
;;
(statistics 1000999 10)
;;
;; Statistics for the prime 1000999
;; (2 3 3 3 3 3 2 3 2 2)
;; Mean: 2.6 milliseconds
;; Std Dev: 0.5163977794943223
;;

(statistics 1000000007 10)
;;
;; Statistics for the prime 1000000007
;; (98 99 124 98 99 94 97 120 95 95)
;; Mean: 101.9 milliseconds
;; Std Dev: 10.774970997640782
;;