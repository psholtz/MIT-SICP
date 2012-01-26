
;;
;; Define "accumulate" procedure:
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;
;; Define "flatmap" procedure:
;;
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;
;; Define "empty-board":
;;
(define empty-board '())

;;
;; Define "queens" procedure:
;;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions (safe? k positions))
	   (flatmap
	    (lambda (rest-of-queens)
	      (map (lambda (new-row)
		     (adjoin-position new-row k rest-of-queens))
		   (enumerate-interval 1 board-size)))
	    (queen-cols (- k 1)))))))
  (queen-cols board-size))

;;
;; Procedure is not quite done yet. We still 
;; need to define "safe?" procedure and 
;; "adjoin-position" procedure:
;;
(define (safe? k positions)
  '())

(define (adjoin-position new-row k rest-of-queens)
  '())