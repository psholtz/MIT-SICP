;;
;; Exercise 5
;; 
;; Enforce a credit limit by taking in a schedule, and removing classes until the total number
;; of units is less than max-credits.
;;
(define (credit-limit schedule max-credits)
  (define (iter elems)
    (if (null? elems)
	'()
	(let ((class (car elems))
	      (credits (total-scheduled-units elems)))
	  (if (>= credits max-credits)
	      (iter (drop-class elems (get-class-number class)))
	      elems))))
  (iter schedule))

;;
;; Run some unit tests:
;;
schedule
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))
(total-scheduled-units schedule)
;; ==> 30

(credit-limit schedule 40)
;; ==> ((calc-201 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))
(credit-limit schedule 31)
;; ==> ((calc-201 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))
(credit-limit schedule 30)
;; ==> ((algb-152 (3 3 3)) (diff-201 (3 3 3)))
(credit-limit schedule 20)
;; ==> ((algb-152 (3 3 3)) (diff-201 (3 3 3)))
(credit-limit schedule 5)
;; ==> ()
(credit-limit schedule 0)
;; ==> ()

;; ** Working --> order of growth 