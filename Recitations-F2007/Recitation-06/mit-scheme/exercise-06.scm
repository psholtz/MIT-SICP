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
  (define (total-scheduled-units-iter seq total)
    (if (null? seq)
	total
	(let ((class (car seq)))
	        (total-scheduled-units-iter (cdr seq) (+ total (get-class-total-units class))))))
  (total-scheduled-units-iter schedule 0))

(define (drop-class schedule classnum)
  (let ((temp-class (make-class classnum '())))
    (define (predicate class)
      (not (same-class? class temp-class)))
    (filter predicate schedule)))

(define (credit-limit schedule max-credits)
  (define (credit-limit-iter elems)
    (if (null? elems)
	'()
	(let ((class (car elems))
	            (credits (total-scheduled-units elems)))
	    (if (>= credits max-credits)
		      (credit-limit-iter (drop-class elems (get-class-number class)))
		            elems))))
  (credit-limit-iter schedule))

;;
;; Basic Classes
;;
(define calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(define calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(define algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(define diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

;;
;; Exercise 6
;;
;; Finish the call to "make-student" to require the student takes at least 1 classs.
;;
;; (make-student 575904467 ...)
;;

;;
;; First we need a constructor/factory for creating schedule checkers 
;; that will validate that the student has elected at least one class.
;;
(define (make-schedule-checker-1)
  (lambda (schedule) 
    (> (length schedule) 0)))

;;
;; Let's run some unit tests:
;;
(define s1 (empty-schedule))
(define s2 (empty-schedule))
(define s2 (add-class calc1 s2))
(define s2 (add-class algebra s2))
(define s2 (add-class diff-eqs s2))

(define sid1 575904476)

(define student1 (make-student sid1 (make-schedule-checker-1)))

;;
;; Now try updating the student with schedules 1 and 2, respectively:
;;
(update-student-schedule student s1)
;; ==> "Invalid schedule!"

(update-student-schedule student s2)
;; ==> (575904476 ((calc-101 (4 4 4)) (algb-152 (3 3 3)) (diff-201 (3 3 3))) #[compound-procedure 36])