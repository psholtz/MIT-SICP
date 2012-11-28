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

;;
;; Exercise 3
;;
;; Write a selector that takes in a schedule and returns the total number of units in that schedule
;;
(defun total-scheduled-units (schedule)
  (defun iter (seq total)
    (if (null seq)
	total
      (let ((class (car seq)))
	(iter (cdr seq) (+ total (get-class-total-units class))))))
  (iter schedule 0))

;;
;; Run some unit tests:
;;
(setq calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(setq calculus-2 (make-class 'CALC-102 (make-units 4 4 4)))
(setq algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(setq diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(get-class-total-units calculus-1)
;; ==> 12
(get-class-total-units calculus-2)
;; ==> 12 
(get-class-total-units algebra)
;; ==> 9
(get-class-total-units diff-eqs)
;; ==> 9

(setq s1 (empty-schedule))
(total-scheduled-units s1)
;; ==> 0

(setq s1 (add-class calculus-1 s1))
(total-scheduled-units s1)
;; ==> 12

(setq s1 (add-class algebra s1))
(total-scheduled-units s1)
;; ==> 21

(setq s1 (add-class diff-eqs s1))
(total-scheduled-units s1)
;; ==> 30

;;
;; The order of the growth is time in linear in the size of the 
;; variable "schedule", since we have to iterate over the entire
;; length of the variable to count all the units. Calculating the 
;; number of units at each node of "schedule" is a constant-time
;; operation, and hence, the order of growth in time is O(n), where
;; n is the size of the variable "schedule".
;;
;; The order of growth in space is linear in the size of the 
;; variable "schedule", presuming that we do not have to make 
;; new copies of the variable at each invocation of the iteration 
;; procedure; that is, presuming that the (cdr seq) call does not 
;; create a copy of the sequence. If no copies are made, then only
;; the first copy of the variable "schedule" is used, and so the 
;; order of growth in space is O(n), or linear in the size of the 
;; variable "schedule".
;;
;; If copies of the variable are made at each procedure invocation, 
;; then the order of growth statistics are O(n^2), where n is the 
;; size of the variable "schedule". Specifically, n(n+1)/2, or roughly
;; (1/2)n^2 units of memory must be allocated to accomodate execution 
;; of the procedure, since first then size-n "schedule" variable is 
;; used, then the size-(n-1) "cdr" of that variable is used, and so on, 
;; down to size 1. 
;; 
