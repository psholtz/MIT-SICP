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

;;
;; Exercise 2
;;
;; Write a selector that when given a class and a schedule, returns a new schedule
;; including the new class
;;
(defun add-class (class schedule)
  (append schedule (list class)))

;;
;; Run some unit tests.
;;
;; First define some units and classes:
;;
(setq u1 (make-units 3 3 3))
(setq calc1 (make-class 101 u1))
(setq calc2 (make-class 102 u1))

;;
;; Now try to build a schedule using these:
;;
(setq s (add-class calc1 (empty-schedule)))
;; ==> ((101 (3 3 3)))
(setq s (add-class calc2 s))
;; ==> ((101 (3 3 3)) (102 (3 3 3)))

;;
;; Inspect the schedule:
;;
(car s)
;; ==> (101 (3 3 3))

(cadr s)
;; ==> (102 (3 3 3))

;;
;; The order of growth in both time and space is linear in the variable
;; "schedule", that is, it is O(n) where "n" is the length of the list
;; structure "schedule".
;;