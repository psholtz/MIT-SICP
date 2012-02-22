;;
;; Exercise 3
;;
;; Write a selector that takes in a schedule and returns the total number of units in that schedule
;;
(define (total-scheduled-units sched)
  (define (iter seq total)
    (cond ((null? seq) total)
	  (else
	   (iter (cdr seq) (+ total (get-class-units (car seq)))))))
  (iter sched 0))

;; ** working --> order of growth