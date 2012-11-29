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

;;
;; Exercise 6
;;
;; Finish the call to "make-student" to require the student takes at least 1 classs.
;;
;; (make-student 575904467 ...)
;;