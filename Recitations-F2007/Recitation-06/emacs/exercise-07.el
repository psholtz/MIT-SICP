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
;; Working definitions (HOPs)
;;
(defun make-student (number sched-checker)
  (list number (list) sched-checker))
(defun get-student-number (x) (car x))
(defun get-student-schedule (x) (cadr x))
(defun get-student-checker (x) (cadr (cdr x)))

(defun update-student-schedule (student schedule)
  (let ((test-function (get-student-checker student)))
    (if (funcall test-function schedule)
	(list (get-student-number student)
	            schedule
		          test-function)
      (error "Invalid schedule!"))))

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

(defun make-schedule-checker-1 ()
  (lambda (schedule)
    (> (length schedule) 0)))

;;
;; Basic Classes
;;
(setq calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(setq calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(setq algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(setq diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))
(setq us-history (make-class 'HIST-122 (make-units 4 4 4)))
(setq world-history (make-class 'HIST-324 (make-units 4 4 4)))
(setq basket-weaving (make-class 'BASKETS (make-units 1 1 1)))

;;
;; Exercise 7
;;
;; Finish the call to "make-student" to create a first-term freshman (limited to 54 units).
;;

;;
;; Define the appropriate constructor.
;;
;; IMPORTANT NOTE: Emacs does not play nicely with HOPs, and to make this work 
;; (using the pattern we defined in the Scheme solution), we need to resort 
;; to using Lisp Macros.
;;
(defmacro make-schedule-checker-2 (var)
  (list 'lambda (list 'schedule) (list '<= (list 'total-scheduled-units 'schedule) var)))

(macroexpand '(make-schedule-checker-2 54))
;; ==> (function (lambda (schedule) (<= (total-scheduled-units schedule) 54)))

;;
;; Let's run some unit tests:
;;
(setq s1 (empty-schedule))
(setq s2 (empty-schedule))
(setq s2 (add-class calc1 s2))
(setq s2 (add-class algebra s2))
(setq s2 (add-class diff-eqs s2))
(setq s3 s2)
(setq s3 (add-class us-history s3))
(setq s3 (add-class world-history s3))
(setq s4 s3)
(setq s4 (add-class basket-weaving s4))

(total-scheduled-units s1)
;; ==> 0
(total-scheduled-units s2)
;; ==> 30
(total-scheduled-units s3)
;; ==> 54
(total-scheduled-units s4)
;; ==> 57

(setq sid1 575904476)

;;
;; IMPORTANT NOTE: We can invoke the routine here using the identical syntax
;; as in the Scheme example. Lisp macros takes care of the syntactic details for us!
;;
(setq student1 (make-student sid1 (make-schedule-checker-2 54)))

;;
;; Now try updating the student with the respective schedules:
;;
(update-student-schedule student1 s1)
;; ==> (575904476 nil (lambda (schedule) (<= (total-scheduled-units schedule) 54)))
(update-student-schedule student1 s2)
;; ==> (575904476 ((CALC-101 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3))) (lambda (schedule) (<= (total-schedul\
ed-units schedule) 54)))
(update-student-schedule student1 s3)
;; ==> (575904476 ((CALC-101 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)) (HIST-122 (4 4 4)) (HIST-324 (4 4 4)))\
 (lambda (schedule) (<= (total-scheduled-units schedule) 54)))
(update-student-schedule student1 s4)
;; ==> "Invalid schedule!"