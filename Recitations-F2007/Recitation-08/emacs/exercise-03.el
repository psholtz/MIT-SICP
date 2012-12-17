;;
;; Working definitions
;;
(defun or? (exp)
  (and (listp exp) (eq (car exp) 'or)))
(defun make-or (exp1 exp2)
  (list 'or exp1 exp2))
(defun or-first (exp)
  (car (cdr exp)))
(defun or-second (exp)
  (car (cdr (cdr exp))))

(defun and? (exp)
  (and (listp exp) (eq (car exp) 'and)))
(defun make-and (exp1 exp2)
  (list  'and exp1 exp2))
(defun and-first (exp)
  (car (cdr exp)))
(defun and-second (exp)
  (car (cdr (cdr exp))))

;;
;; Previous Exercises
;;
(defun not? (exp)
  (and (listp exp) (eq (car exp) 'not)))
(defun make-not (exp)
  (list 'not exp))
(defun not-first (exp)
  (car (cdr exp)))

;;
;; Exercise 3
;;
;; Given a boolean expression and a set of variable assignments, evaluate the expression to 
;; decide whether the result if #t or #f. Assume that you have a procedure (variable-value
;; name environment), which takes a variable and and a list of values and returns the value
;; assigned to the variable, if a binding for it exists, or throws an error if no binding is 
;; found.
;;

;;
;; As with some of the other examples in Chapter 2, it's easier to get a handle on these 
;; exercies if we peek ahead to Chapter 3 and use the "table" structure defined there, to 
;; use for our symbol bindings and execution environment.
;;
;; We'll import the relevant table definitions:
;;
(defun assoc (key records)
  (cond ((null records) '())
	((equal key (car (car records))) (car records))
	(t
	 (assoc key (cdr records)))))

(defun lookup (key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
      '())))

(defun insert! (key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(setcdr record value)
      (setcdr (cdr table)
	      (cons (cons key value) (cdr table))))))

(defun make-table ()
  (list '*table*))