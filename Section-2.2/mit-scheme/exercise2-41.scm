;;
;; Exercise 2.41
;;

;;
;; First let's import the "unique-pairs" procedure from Exercise 2.40 
;; (We'll name it the "ordered-pairs" procedure, for consistency).
;;
(define (ordered-pairs n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

;;
;; The supporting procedures we need to make this procedure work are:
;;
(define (enumerate-interval i j)
  (define (iter count total)
    (cond ((<= count j) (iter (+ count 1) (append total (list count))))
	  (else 
	   total)))
  (iter i '()))

(define (accumulate op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;;
;; With the "ordered-pairs" procedure defined, we can now define
;; our "ordered-triples" procedure as follows:
;;
;;; ==> this should be "ordered-triples-for-n"
(define (ordered-triples n)
  (map (lambda (i) (append (list n) i)) (ordered-pairs (- n 1))))

;;
;; Note that it doesn't make sense to think of ordered triples of positive
;; integers less than 3:
;;
(ordered-triples 1)
;; ==> ()
(ordered-triples 2)
;; ==> ()
(ordered-triples 3)
;; ==> ((3 2 1))
(ordered-triples 4)
;; ==> ((4 2 1) (4 3 1) (4 3 2))

;;
;; What we are seeking is all ordered triples, less than or equal to n, 
;; that sum to a given integer s.
;;
(define (target-sum n s)
  '())