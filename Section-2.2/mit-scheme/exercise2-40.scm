;;
;; Exercise 2.40
;;
;; Define a procedure "unique-pairs" that, given an integer n, generates the 
;; sequence of pairs (i,j) with 1 <= j < i <= n. Use "unique-pairs" 
;; to simplify the definition of "prime-sum-pairs" given above.
;;

;;
;; First (re-)define the "accumulate" procedure:
;;
(define (accumulate op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;;
;; Define the "enumerate-interval" procedure:
;;
(define (enumerate-interval i j)
  (define (iter count total)
    (cond ((<= count j) (iter (+ count 1) (append total (list count))))
	  (else 
	   total)))
  (iter i '()))

;;
;; Test the "enumerate-interval" procedure:
;;
(enumerate-interval 1 5)
;; ==> (1 2 3 4 50
(enumerate-interval 1 10)
;; ==> (1 2 3 4 5 6 7 8 9 10)
(enumerate-interval 1 1)
;; ==> (1)
(enumerate-interval 1 2)
;; ==> (1 2)
(enumerate-interval 1 0)
;; ==> ()

;;
;; "unique-pairs" can be defined by simply abstracting the procedure
;; definition already given in the text, which defines the generation of pairs.
;;
(define (unique-pairs n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

;;
;; This definition is workable, but as was pointed out in the text, the combination
;; of mapping and accumulating is so common that it's best to isolate it as its 
;; own separate procedure:
;;
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;;
;; Using this abstraction, we can now write:
;;
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pairs 1)
;; ==> ()
(unique-pairs 2)
;; ==> ((2 1))
(unique-pairs 3)
;; ==> ((2 1) (3 1) (3 2))
(unique-pairs 5)
;; ==> ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;;
;; In order to implement the "prime-sum-pairs" procedure, we need to 
;; implement a prime-testing routine, which we will import from Section 1.2:
;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

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

(prime? 3)
;; ==> #t
(prime? 1000999)
;; ==> #t

;;
;; We also need the supporting procedures defined in the text:
;;
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;
;; With all this machinery in place, we can re-define the "prime-sum-pairs" 
;; procedure as follows:
;;
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;
;; Testing this procedure:
;;
(prime-sum-pairs 6)
;; ==> ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))