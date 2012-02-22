;;
;; Exercise 5
;; 
;; Enforce a credit limit by taking in a schedule, and removing classes until the total number
;; of units is less than max-credits.
;;
(define (credit-limit schedule max-credits)
  (define (iter elems)
    (let ((class (car elems))
	  (credits (total-scheduled-units elems)))
      (if (>= credits max-credits)
	  (iter (drop-class elems (get-class-number class)))
	  elems)))
  (iter schedule))

;; ** Working --> order of growth 