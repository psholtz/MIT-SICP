;;
;; Exercise 1.27
;;
;; Demonstrate that the Carmichael numbers really do fool the Fermat test. That is,
;; write a procedure that takes an integer n and tests whether a^n congruent to 
;; a mod n for every a < n, and try the procedure on the given Charmichael numbers.
;;
;; Charmichael numbers: 561 1105 1729 2465 2821 6601
;;

;; +++++++++++++++++++++++++++++++++++++++ 
;; Procedures to support normal prime test
;; +++++++++++++++++++++++++++++++++++++++ 
(define (smallest-divisor n)
  (if (divides? 2 n)
      2
      (find-divisor n 3)))

(define (next n)
  (+ n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else
	 (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n) (* n n))

(define (prime? n)
  (if (= n (smallest-divisor n))
      #t
      #f))

;; +++++++++++++++++++++++++++++++++
;; Procedures to support Fermat test
;; +++++++++++++++++++++++++++++++++
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (even? n) (= (remainder n 2) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;;
;; We can write some simple "unit tests" to see whether the 
;; two procedures return the same result for small primes:
;;

(define n 100) ;; run the Fermat tests 100 times

(eq? (prime? 3) (fast-prime? 3 n))    ;; t
(eq? (prime? 5) (fast-prime? 5 n))    ;; t
(eq? (prime? 7) (fast-prime? 7 n))    ;; t
(eq? (prime? 11) (fast-prime? 11 n))  ;; t

;;
;; We can run these unit tests on composite numbers as well:
;;
(eq? (prime? 4) (fast-prime? 4 n))     ;; t
(eq? (prime? 6) (fast-prime? 6 n))     ;; t
(eq? (prime? 9) (fast-prime? 9 n))     ;; t
(eq? (prime? 10 ) (fast-prime? 10 n))  ;; t

;; 
;; However when we try running htese unit tests on the Carmichael numbers, 
;; we get a failure condition:
;;
(eq? (prime? 561) (fast-prime? 561 n))     ;; #f
(eq? (prime? 1105) (fast-prime? 1105 n))   ;; #f
(eq? (prime? 1729) (fast-prime? 1729 n))   ;; #f
(eq? (prime? 2465) (fast-prime? 2465 n))   ;; #f
(eq? (prime? 2821) (fast-prime? 2821 n))   ;; #f
(eq? (prime? 6601) (fast-prime? 6601 n))   ;; #f

;;
;; Each of the Carmichael numbers is in fact composite:
;;
(smallest-divisor 561)
;; --> 3
(* 3 187)
;; --> 561

(smallest-divisor 1105)
;; --> 5
(* 5 221)
;; --> 1105

(smallest-divisor 1729)
;; --> 7
(* 7 247)
;; --> 1729

(smallest-divisor 2465)
;; --> 5
(* 5 493)
;; --> 2465

(smallest-divisor 2821)
;; --> 7
(* 7 403)
;; --> 2821

(smallest-divisor 6601)
;; --> 7
(* 7 943)
;; --> 6601

