;;
;; Exercise 4
;;
;; Write a procedure that drops a particular class from a schedule.
;;

;;
;; Scheme ships with a "delete" procedure, which we can use as follows:
;;

(define (drop-class schedule classnum)
  (define (iter elems)
    (if (not (null? elems))
	(let ((class (car elems)))
	  (if (equal? (get-class-number class) classnum)
	      (delete class schedule))
	  (iter (cdr elems)))))
  (iter schedule))

;;
;; Run some unit tests:
;;
(define calculus (make-class 'CALC-101 (make-units 4 4 4)))


;;
;; Working --> orders of growth
;;