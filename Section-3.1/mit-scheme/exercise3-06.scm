;; 
;; Exercise 3.6
;;
;; It is useful to be able to reset a random-number generate to produce a
;; sequence starting from a given value. Design a new "rand" procedure that
;; is called with an argument that is either the symbol "generate" or the 
;; symbol "reset" and behaves as follows: (rand 'generate) produces a new 
;; random number; ((rand 'reset) <new-value>) resets the internal state 
;; variable to the designated <new-value>. Thus, by resetting the state, 
;; one can generate repeatable sequences. These are very handy to have when
;; testing and debugging programs that use random numbers.
;;

;; 
;; One simple way to implement a PRNG is to use a linear congruential 
;; generator (LCG) where x1 = (a*x0 + c) % m. So that the resulting 
;; sequence is sufficiently random, the coefficients a, c and m should
;; be selected so that:
;;
;;  (1) c and m are relatively prime;
;;  (2) a-1 is divisible by all prime factors of m;
;;  (3) a-1 is a multiple of 4 if m is a multiple of 4;
;; 
;; Moreover, the size of m will determine the size of the sample space
;; from which the uniform distribution is chosen.
;;
;; The following LCG is due to Donald Knuth:
;;
(define (rand-update x)
  (let ((a 6364136223846793005)
	(c 1442695040888963407)
	(m (expt 2 64)))
    (modulo (+ (* a x) c) m)))

;;
;; With the PRNG defined, we can specify the dispatch procedure as follows:
;;
(define rand 
  (let ((x 0))  ;; start seed at 0
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? m 'reset) 
	     (lambda (seed)
	       (begin (set! x seed)
		      seed)))
	    (else
	     (error "Unknown message --" m))))
    dispatch))  

;;
;; Start by seeding PRNG at 0:
;;
((rand 'reset) 0)
(rand 'generate)
;; ==> 1442695040888963407 
(rand 'generate)
;; ==> 1876011003808476466
(rand 'generate)
;; ==> 11166244414315200793

;;
;; Try re-seeding the PRNG at 0:
;;
((rand 'reset) 0)
(rand 'generate)
;; ==> 1442695040888963407
(rand 'generate)
;; ==> 1876011003808476466
(rand 'generate)
;; ==> 11166244414315200793