;;
;; Working definitions
;;
(defun make-units (C L H)
 (list C L H))
(defun get-units-C (x) (car x))
(defun get-units-L (x) (cadr x))
(defun get-units-H (x) (cadr (cdr x)))

(defun make-class (number units)
 (list number units))
(defun get-class-number (x) (car x))
(defun get-class-units (x) (cadr x))

(defun get-class-total-units (class)
 (let ((units (get-class-units class)))
  (+ 
   (get-units-C units)
   (get-units-L units)
   (get-units-H units))))

(defun same-class? (c1 c2)
 (equal (get-class-number c1) (get-class-number c2)))

;;
;; Previous solutions
;;
(defun empty-schedule () '())
(defun add-class (class schedule)
  (append schedule (list class)))
(defun total-scheduled-units (schedule)
  (defun iter (seq total)
    (if (null seq)
	total
      (let ((class (car seq)))
	(iter (cdr seq) (+ total (get-class-total-units class))))))
  (iter schedule 0))

;;
;; Use the more complicated definition of "drop-class" so we don't
;; have to re-implement the "filter" procedure:
;;
(defun drop-class (schedule classnum)
  ;;
  ;; "Temp" class is defined so that we can use the 
  ;; procedure "same-class" that is provided in the "API"
  ;;
  (let ((temp-class (make-class classnum '())))
    (defun diter (elems)
      (if (null elems)
	    '()
	(let ((class (car elems)))
	    (if (same-class? class temp-class)
		      (diter (cdr elems))
	          (append (list class) (diter (cdr elems)))))))
    (diter schedule)))

;;
;; Exercise 5
;; 
;; Enforce a credit limit by taking in a schedule, and removing classes until the total number
;; of units is less than max-credits.
;;
(defun credit-limit (schedule max-credits)
  (defun citer (elems)
    (if (null elems)
	'()
      (let ((class (car elems))
	    (credits (total-scheduled-units elems)))
	(if (> credits max-credits)
	    (citer (drop-class elems (get-class-number class)))
	  elems))))
  (citer schedule))

;;
;; Run some unit tests:
;;
(setq calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(setq calculus-2 (make-class 'CALC-102 (make-units 4 4 4)))
(setq algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(setq diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(setq s1 (empty-schedule))
(setq s1 (add-class calculus-1 s1))
(setq s1 (add-class calculus-2 s1))
(setq s1 (add-class algebra s1))
(setq s1 (add-class diff-eqs s1))

;;
;; Introspect s1:
;;
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))
;;

;;
;; Total number of units in s1:
;;
(total-scheduled-units s1)
;; ==> 42

;;
;; First test the empty case:
;;
(credit-limit '() 10)
;; ==> nil

;;
;; Then test the "do nothing" case:
;;
(credit-limit s1 50)
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 42)
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 41)
;; ==> ((CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 25)
;; ==> ((ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(total-scheduled-units (credit-limit s1 42))
;; ==> 42
(total-scheduled-units (credit-limit s1 41))
;; ==> 30
(total-scheduled-units (credit-limit s1 30))
;; ==> 30
(total-scheduled-units (credit-limit s1 25))
;; ==> 18

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