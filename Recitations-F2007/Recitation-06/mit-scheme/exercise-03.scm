;;
;; Exercise 3
;;
;; Write a selector that takes in a schedule and returns the total number of units in that schedule
;;
(define (total-scheduled-units schedule)
  (define (iter seq total)
    (if (null? seq)
	total
	(let ((class (car seq)))
	  (iter (cdr seq) (+ total (get-class-total-units class))))))
  (iter schedule 0))

;; ** working --> order of growth