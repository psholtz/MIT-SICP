;;
;; Exercise 2.43
;;
;; [WORKING]
;;

;;
;; Supporting procedures:
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	    (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))



;;
;; Application of flatmap from Exercise 2.42:
;;
(flatmap 
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row k rest-of-queens))
	(enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))

;;
;; Using this procedure, one invocation of "flatmap" will execute 
;; the following procedures the following number of times:
;;
;;  (queen-cols (- k 1)) ==> 1 call
;;  (adjoin-position new-row k rest-of-queens) ==> # of elements in (queen-cols (- k 1))
;;  (enumerate-interval 1 board-size) ==> 1 call
;;
;; Note that "adjoin-position", which is invoked the most frequently, is 
;; a relatively cheap operation, only making a linear-time call to "cons".
;;

;;
;; Louis Reasoner's application of flatmap:
;;
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;;
;; Using this procedure, one invocation of "flatmap" will execute
;; the following procedures the following number of times:
;;
;;  (enumerate-interval 1 board-size) ==> 1 call
;;  (queen-cols (- k 1) ==> 1 call




;;
;; Let's step through some call graphs.
;;
;; As we showed in the previous exercise, if the board size is 4
;; then (queen-cols 3) will be:
;;
;;  ((1 4 2) (2 4 1) (3 1 4) (4 1 3))
;;
;; So the call to flatmap, at the iteration with k=4, might look something like:
;;
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row 4 rest-of-queens))
	(enumerate-interval 1 board-size)))
 '((1 4 2) (2 4 1) (3 1 4) (4 1 3)))

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row 4 rest-of-queens))
	'(1 2 3 4)))
 '((1 4 2) (2 4 1) (3 1 4) (4 1 3)))

'((1 1 4 2) (2 1 4 2) (3 1 4 2) (4 1 4 2)
 (1 2 4 1) (2 2 4 1) (3 2 4 1) (4 2 4 1)
 (1 3 1 4) (2 3 1 4) (3 3 1 4) (4 3 1 4)
 (1 4 1 3) (2 4 1 3) (3 4 1 3) (4 4 1 3))

;;
;; Let's look at what the call graph looks like using Louis Reasoner's approach:
;;
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row 4 rest-of-queens))
	'((1 4 2) (2 4 1) (3 1 4) (4 1 3))))
 '(1 2 3 4))