;;
;; Exercise 4
;;
;; Write a procedure that drops a particular class from a schedule.
;;

(define (drop-class sched classnum)
  (define (iter elems)
    (if (null? elems)
	'()
	(let ((class (car elems)))
	  (if (equal (get-class-number class) classnum)
	      '()
	      (iter (cdr elems))))))
  (iter sched))

;;
;; Working --> orders of growth
;;