;;
;; Working definitions
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

;;
;; Exercise 5
;; 
;; Enforce a credit limit by taking in a schedule, and removing classes until the total number
;; of units is less than max-credits.
;;
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

;;
;; In a worst-case scenario, we need to step through all "n" elements of
;; the schedule structure, and at each step, we need to  invoke the
;; "total-scheduleded-units" procedure, which itself runs on O(n) time.
;; Walking down the list structure costs "n" steps, and invoking
;; "total-scheduled-units" at each nodes costs a total of an additional
;; (1/2)*(n)*(n+1) steps, so the total number of steps involved thus
;; far is (1/2)*(n^2+3*n).
;;
;; Furthermore, in a worst-case scenario we need to invoke the "drop-class"
;; procedure at each node, which is also linear in "n". Invoking this linear-time
;; procedure at each step of the structure will add an additional n*(n+1)/2
;; steps to the computation. The total number of steps required (in a worst
;; case scenario) will be n^2 + 2*n, so the procedure (in a worst-case
;; scenario) will run in O(n^2) time.
;;
;; In most instances, the procedure will run much more quickly than this.
;;
;; To calculate the space requirements, let's assume that the "total-scheduled-units"
;; procedure requires O(n) linear space. In a worst case scenario, we need to create
;; a new copy of the schedule, of size (n-1), at each step of the procedure
;; using the "drop-class" procedure. "drop-class" is linear in space, but
;; creating a new copy of the structure, of size (n-1), at each step, will require
;; a total of n(n+1)/2 units of memory. Hence the space requirements for the
;; algorithm - in a worst-case scenario - are O(n^2).
;;