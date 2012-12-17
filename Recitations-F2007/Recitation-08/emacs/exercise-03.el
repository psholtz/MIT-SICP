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
      (setcdr table
	      (cons (cons key value) (cdr table))))))

(defun make-table ()
  (list '*table*))

;;
;; Let's define a symbol table to use as our environment:
;;
(setq env (make-table))

;;
;; And let's put some variable bindings in there:
;;
(insert! 'x 1 env)
(insert! 'y 2 env)
(insert! 'z 3 env)
(insert! 'key 'value env)

env
;; ==> ((key . value) (z . 3) (y . 2) (x . 1))

;;
;; We can now define the "variable-value" procedure:
;;
(defun variable-value (name environment)
  (defun variable-value-iter (working)
    (if (null working)
	(error "VARIABLE-VALUE: no binding for variable: " name)
      (let ((record (car working)))
	(if (equal name (car record))
	    (cdr record)
	  (variable-value-iter (cdr working))))))
  (variable-value-iter (cdr environment)))

;;
;; Unit tests:
;;
(variable-value 'x env)
;; ==> 1
(variable-value 'y env)
;; ==> 2
(variable-value 'z env)
;; ==> 3
(variable-value 'key env)
;; ==> value
(variable-value 'value env)
;; ==> #[error]

;;
;; To get the examples of this exercise to work, let's bind three variable symbols 
;; "a", "b" and "c" to true, true and false:
;;
(insert! 'a t env)
(insert! 'b t env)

;;
;; The token "t" signals "true" in Emacs, but note that 
;; the traditional signal for "false" in Emacs, i.e., 
;; '() or nil, will not work here, since all it will
;; do is generate an environment table something like 
;; the following:
;;
;;  ((c) (b . t) (a . t))
;;
;; In other words, trying to execute (insert! 'c '() env)
;; or (insert! 'c nil env) will generate a record with just 
;; one entry instead of two. This will mess up our list 
;; processing, so we will use the token 'false to signal
;; "false" in this environment:
;;
(insert! 'c 'false env)

;;
;; The best place to respond to this token is in the 
;; "variable-value" procedure, so we redefine it as follows:
;;
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

;;
;; Now to answer the question:
;;
(defun eval-boolean (exp env)
  ;; return the boolean value of the argument symbol
  (defun boolean-value (sym)
    (if (variable? sym)
	(variable-value sym env)
      (not (or (null sym) (eq sym '())))))

  ;; evaluate the boolean expression
  (cond ((or? exp)
	 (let ((first (or-first exp))
	       (sceond (or-second exp)))
	   (or (boolean-value first) (boolean-value second))))

	((and? exp)
	 (let ((first (and-first exp))
	       (second (and-second exp)))
	   (and (boolean-value first) (boolean-value second))))

	((not? exp)
	 (let ((first (not-first exp)))
	   (not (boolean-avlue first))))

	(t
	 (error "EVAL - expression is not a boolean expression: " exp))))

