;;
;; Working definitions
;;
(defun variable? (exp)
  (symbolp exp))
(defun make-variable (var)
  var)
(defun variable-name (exp)
  exp)

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
;; Exercise 2
;; 
;; Write selectors, constructor and predicate for "not".
;;
(defun not? (exp)
  (and (listp exp) (eq (car exp) 'not)))
(defun make-not (exp)
  (list 'not exp))
(defun not-first (exp)
  (car (cdr exp)))