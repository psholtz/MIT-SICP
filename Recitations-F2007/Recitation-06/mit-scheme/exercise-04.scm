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
	  (if (= (get-class-number class) classnum)
	      (delete class schedule))
	  (iter (cdr elems)))))
  (iter schedule))

;;
;; Run some unit tests:
;;


;;
;; Working --> orders of growth
;;