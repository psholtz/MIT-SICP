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
;; Exercise 4
;;
;; Write a procedure that drops a particular class from a schedule.
;;

;;
;; One way to implement the procedure would be as follows:
;;
(defun drop-class (schedule classnum)
  ;;
  ;; "Temp" class is defined so that we can use the 
  ;; procedure "same-class" that is provided in the "API"
  ;;
  (let ((temp-class (make-class classnum '())))
    (defun iter (elems)
      (if (null elems)
	  '()
	(let ((class (car elems)))
	  (if (same-class? class temp-class)
	      (iter (cdr elems))
	    (append (list class) (iter (cdr elems)))))))
    (iter schedule)))

;;
;; A more efficient way to implement the "drop-class" procedure would be to use "filter".
;;
;; Since emacs does not ship with "filter", the way other Lisps do, we first define our own:
;;
(require 'cl)
(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun drop-class (schedule classnum)
  ;;
  ;; "Temp" class is defined so that we can use the 
  ;; procedure "same-class" that is provided in the "API"
  ;;
  (let ((temp-class (make-class classnum '())))
    (defun predicate (class)
      (not (same-class? class temp-class)))
    (filter 'predicate schedule)))

;;
;; Run some unit tests:
;;
(setq calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(setq algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(setq diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(get-class-total-units calculus-1)
;; ==> 12
(get-class-total-units algebra)
;; ==> 9
(get-class-total-units diff-eqs)
;; ==> 9

(setq schedule (empty-schedule))
(total-scheduled-units schedule)
;; ==> 0

(setq schedule (add-class calculus-1 schedule))
(total-scheduled-units schedule)
;; ==> 12 

(setq schedule (add-class algebra schedule))
(total-scheduled-units schedule)
;; ==> 21

(setq schedule (add-class diff-eqs schedule))
(total-scheduled-units schedule)
;; ==> 30

;;
;; Test the "drop-class" procedure:
;;
(drop-class schedule 'XXX)
;; ==> ((CALC-101 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(drop-class schedule 'CALC-101)
;; ==> ((ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(drop-class schedule 'ALGB-152)
;; ==> ((CALC-101 (4 4 4)) (DIFF-201 (3 3 3)))

(drop-class schedule 'DIFF-201)
;; ==> ((CALC-101 (4 4 4)) (ALGB-152 (3 3 3)))

;;
;; The order of growth for the more efficient version of "drop-class"
;; which uses the "filter" procedure is linear in both space and time, 
;; that is, O(n) where n is the length of the list structure "schedule".
;;

;;
;; The order of growth for the less efficient version of "drop-class",
;; the one which does not use the "filter" procedure, is O(n^2) in time.
;; In a worst-case scenario, the procedure must step through each element
;; of the schedule structure, and call the "append" procedure, which 
;; likewise is O(n). The result is a procedure call that uses n(n+1)/2 steps.
;;

;;
;; The order of growth for the less efficient version of "drop-class",
;; the one which does not use the "filter" procedure, is O(n) in space.
;; The procedure uses on copy of the "schedule" data structure as an 
;; argument, and it dynamically builds a second copy of the structure 
;; using recursive calls to append. The result is a procedure call that 
;; requires 2*n units of memory.
;;