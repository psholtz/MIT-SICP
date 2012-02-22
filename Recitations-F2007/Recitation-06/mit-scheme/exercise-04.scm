;;
;; Exercise 4
;;

;;
;; Write a procedure that drops a particular class from a schedule.
;;

;;
;; Scheme ships with a "delete" procedure, which we can use as follows:
;;
(define (drop-class schedule classnum)
  (define (iter elems)
    (if (null? elems)
	'()
	(let ((class (car elems)))
	  (if (equal? (get-class-number class) classnum)
	      (iter (cdr elems))
	      (append (list class) (iter (cdr elems)))))))
  (iter schedule))

;;
;; Run some unit tests:
;;
(define calculus (make-class 'CALC-101 (make-units 4 4 4)))
(define algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(define diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(get-class-total-units calculus)
;; ==> 12
(get-class-total-units algebra)
;; ==> 9
(get-class-total-units diff-eqs)
;; ==> 9

(define schedule (empty-schedule))
(total-scheduled-units schedule)
;; ==> 0

(define schedule (add-class calculus schedule))
;; ==> ((calc-101 (4 4 4)))
(total-scheduled-units schedule)
;; ==> 12

(define schedule (add-class algebra schedule))
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)))
(total-scheduled-units schedule)
;; ==> 21

(define schedule (add-class diff-eqs schedule))
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))
(total-scheduled-units schedule)
;; ==> 30

;;
;; Test the "drop-class" procedure:
;;
(drop-class schedule 'xxx)
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))

(drop-class schedule 'calc-101)
;; ==> ((algb-152 (3 3 3)) (diff-201 (3 3 3)))

(drop-class schedule 'algb-152)
;; ==> ((calc-101 (4 4 4)) (diff-201 (3 3 3)))

(drop-class schedule 'diff-201)
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)))

;;
;; Working --> orders of growth
;;