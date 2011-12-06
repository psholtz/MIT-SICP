;;
;; Exercise 2.40
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