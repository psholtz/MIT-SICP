;;
;; Exercise 1
;;
;; Write a function "occurrences" that takes a number and a list and counts the number
;; of times the number appears in the list. Write two versions -- one that uses "filter"
;; and one that uses "fold-right". For example:
;;
;; (occurrences 1 (list 1 2 1 1 3)) ==> 3
;;

;;
;; First let's define "occurrences" in a "naive" way, using simple recursion:
;;
(define (occurrences num a)
  (if (null? a)
      0
      (if (= num (car a))
	  (+ 1 (occurrences num (cdr a)))
	  (occurrences num (cdr a)))))

;;
;; Unit tests:
;;
(occurrences 1 (list 1 2 1 1 3))
;; ==> 3

(occurrences 2 (list 1 2 1 1 3))
;; ==> 1

(occurrences 3 (list 1 2 1 1 3))
;; ==> 1

(occurrences 4 (list 1 2 1 1 3))
;; ==> 0

;;
;; Now write the definition using the "filter" procedure:
;;
(define (occurrences num a)
  (define (predicate x)
    (if (= x num)
	#t
	#f))
  (length (filter predicate a)))

;;
;; Running the unit tests defined above gives the same results.
;;

;;
;; Now write the definition using the "fold-right" procedure:
;;
(define (occurrences num a)
  (define (func x y)
    (if (= x num)
	(append y (list x))
	y))
  (length (fold-right func '() a)))

;;
;; Running the unit tests defined above gives the same results.
;;

;;
;; Let's expand the call graph for (occurrences 1 (list 1 2 1 1 3)), 
;; at least insofar as the calls to "fold-right" are concerned:
;;

(define (func x y)
  (let ((num 1))  ;; assign a value for 1 for heuristic purposes
    (if (= x num)
	(append y (list x))
	y)))

(fold-right func '() '(1 1 2 1 3))
(func 1 (fold-right func '() '(1 2 1 3)))
(func 1 (func 1 (fold-right func '() '(2 1 3))))
(func 1 (func 1 (func 2 (fold-right func '() '(1 3)))))
(func 1 (func 1 (func 2 (func 1 (fold-right func '() '(3))))))
(func 1 (func 1 (func 2 (func 1 (func 3 (fold-right func '() '()))))))
(func 1 (func 1 (func 2 (func 1 (func 3 '())))))
(func 1 (func 1 (func 2 (func 1 '()))))
(func 1 (func 1 (func 2 '(1))))
(func 1 (func 1 '(1)))
(func 1 '(1 1))
'(1 1 1)

;;
;; We then take the "length" of this list to arrive at the final answer.
;;

