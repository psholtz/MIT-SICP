;;
;; Working definitions (Data Structures)
;;
(define (make-units C L H)
 (list C L H))
(define get-units-C car)
(define get-units-L cadr)
(define get-units-H caddr)

(define (make-class number units)
 (list number units))
(define get-class-number car)
(define get-class-units cadr)

(define (get-class-total-units class)
 (let ((units (get-class-units class)))
  (+ 
   (get-units-C units)
   (get-units-L units)
   (get-units-H units))))

(define (same-class? c1 c2)
 (equal? (get-class-number c1) (get-class-number c2)))

;;
;; Working definitions (HOPs)
;;
(define (make-student number sched-checker)
  (list number (list) sched-checker))
(define get-student-number car)
(define get-student-schedule cadr)
(define get-student-checker caddr)

(define (update-student-schedule student schedule)
  (if ((get-student-checker student) schedule)
      (list (get-student-number student)
	                schedule
			        (get-student-checker student))
      (error "Invalid schedule!")))

;;
;; Previous solutions
;;
(define (empty-schedule) '())

(define (add-class class schedule)
  (append schedule (list class)))

(define (total-scheduled-units schedule)
  (define (iter seq total)
    (if (null? seq)
	total
	(let ((class (car seq)))
	            (iter (cdr seq) (+ total (get-class-total-units class))))))
  (iter schedule 0))

(define (drop-class schedule classnum)
  (let ((temp-class (make-class classnum '())))
    (define (predicate class)
      (not (same-class? class temp-class)))
    (filter predicate schedule)))

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

(define (make-schedule-checker-1)
  (lambda (schedule) 
    (> (length schedule) 0)))

(define (make-schedule-checker-2)
  (lambda (schedule)
    (<= (total-scheduled-units schedule) 54)))

;;
;; Basic Classes
;;
(define calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(define calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(define algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(define diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))
(define us-history (make-class 'HIST-122 (make-units 4 4 4)))
(define world-history (make-class 'HIST-324 (make-units 4 4 4)))
(define basket-weaving (make-class 'BASKETS (make-units 1 1 1)))

;;
;; Exercise 9
;;
;; Rewrite "drop-class" to use "filter".
;;

;;
;; We had already written "drop-class" to use filter:
;;
(define (drop-class schedule classnum)
  (let ((temp-class (make-class classnum '())))
    (define (predicate class)
      (not (same-class? class temp-class)))
    (filter predicate schedule)))

;;
;; Run some unit tests:
;;
(define s2 (empty-schedule))
(define s2 (add-class calc1 s2))
(define s2 (add-class algebra s2))
(define s2 (add-class diff-eqs s2))

;;
;; Introspect s2:
;;
s2
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))                                                
(drop-class s2 'CALC-101)
;; ==> ((algb-152 (3 3 3)) (diff-201 (3 3 3)))

;;
;; The "non-filter" version is given as:
;;
(define (drop-class schedule classnum)
  (let ((temp-class (make-class classnum '())))
    (define (iter elems)
      (if (null? elems)
	  '()
	  (let ((class (car elems)))
	    (if (same-class? class temp-class)
		(iter (cdr elems))
		(append (list class) (iter (cdr elems)))))))
    (iter schedule)))

;;
;; Introspect s2:
;;
s2 
;; ==> ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3)))

(drop-class s2 'CALC-101)
;; ==> ((algb-152 (3 3 3)) (diff-201 (3 3 3)))