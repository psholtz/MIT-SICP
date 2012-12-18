;;
;; Working definitions
;;
(defun variable? (exp)
  (cond ((eq exp t) nil)
	((eq exp nil) nil)
	((eq exp '()) nil)
	(t
	  (symbolp exp))))
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

(defun not? (exp)
  (and (listp exp) (eq (car exp) 'not)))
(defun make-not (exp)
  (list 'not exp))
(defun not-first (exp)
  (car (cdr exp)))

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
      (setcdr table
	      (cons (cons key value) (cdr table))))))

(defun make-table ()
  (list '*table*))

(setq env (make-table))

(defun variable-value (name environment)
  (defun variable-value-iter (working)
    (if (null working)
	(error "VARIABLE-VALUE: no binding for variable: " name)
      (let ((record (car working)))
	(if (equal name (car record))
	    (if (eq (cdr record) 'false)
		'()
	      (cdr record))
	  (variable-value-iter (cdr working))))))
  (variable-value-iter (cdr environment)))

(defun eval-boolean (exp env)
  ;; return the boolean value of the argument symbol
  (defun boolean-value (sym)
    (if (variable? sym)
	(variable-value sym env)
      (not (or (null sym) (eq sym '())))))
  
  ;; evaluate the boolean expression
  (cond ((or? exp)
	 (let ((first (or-first exp))
	       (second (or-second exp)))
	   (or (boolean-value first) (boolean-value second))))
	
	((and? exp)
	 (let ((first (and-first exp))
	       (second (and-second exp)))
	   (and (boolean-value first) (boolean-value second))))
	
	((not? exp)
	 (let ((first (not-first exp)))
	   (not (boolean-value first))))
	
	(t
	 (error "EVAL - expression is not a boolean expression: " exp))))
