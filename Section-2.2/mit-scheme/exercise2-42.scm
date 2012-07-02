

;;
;; First define the supporting procedures required by "queens".
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
  (accumulate append '() (map proc seq)))

;;
;; Define "enumerate-interval" procedure:
;;
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;
;; Define "queens" procedure:
;;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;;
;; To make this work, we need to implement the "safe?" and 
;; "adjoin-position" procedures. In order to do this, we 
;; need to form some conception of what, precisely, a "position" 
;; on the chess board is. 
;;
;; We define the following data structure, with supporting accessors:
;;
(define (make-position row col)
  (cons row col))
(define (position-row position)
  (car position))
(define (position-col position)
  (cdr position))
(define (positions-equal? a b)
  (equal a b))

;;
;; At this point, we can also define the "empty-board" constant:
;;
(define empty-board '())


;;
;; [WORKING]
;;
(define (adjoin-position new-row column rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? column positions)
  (define (next-column-safe? new-row positions row-offset)
    (if (null? positions)
	true
	(let ((this-row (car positions)))
	  (if (or (= this-row new-row)
		  (= (+ this-row row-offset) new-row)
		  (= (- this-row row-offset) new-row))
	      false
	      (next-column-safe? new-row (cdr positions) (+ 1 row-offset))))))
  (next-column-safe? (car positions) (cdr positions) 1))
