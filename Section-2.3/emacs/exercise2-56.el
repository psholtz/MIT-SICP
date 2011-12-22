;;
;; Exercise 2.56
;;
;; Show how to extend the basic differentiator to handle more kinds of expressions.
;; For instance, implement the differentiation rule:
;;
;; d/dx(u^n) = n*u^(n-1) * du/dx
;;
;; by adding a new clause to the "deriv" program and defining appropriate procedures
;; "exponentiation?", "base", "exponent" and "make-exponentiatoin". You may use the 
;; symbol ** to denote exponentiation). Build in the rules that anything raised to the 
;; power 0 is 1 and anything raised to the power 1 is itself.
;;

;;
;; First let's import the symbolic differentiation package as expressed in the text.
;;
;; We start with the selectors needed to support the "deriv" procedure.
;;
;; First the procedures that support basic symbol manipulation:
;;
(defun variable? (x) (symbolp x))

(variable? 'x)
;; ==> t
(variable? 31)
;; ==> nil

(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (eq v1 v2)))

(same-variable? 10 30)
;; ==> nil
(same-variable? 10 'x)
;; ==> nil
(same-variable? 'x 10)
;; ==> nil
(same-variable? 'x 'y)
;; ==> nil
(same-variable? 'x 'x)
;; ==> t

(defun =number? (exp num)
  (and (numberp exp) (= exp num)))

(setq test-value-01 0)
;; ==>
(setq test-value-02 10)
;; ==>
(=number? 'x 0)
;; ==>
(=number? 0 0)
;; ==>
(=number? 10 0)
;; ==>
(=number? test-value-01 0)
;; ==>
(=number? test-value-01 10)
;; ==>
(=number? test-value-02 0)
;; ==>
(=number? test-value-02 10)
;; ==>

;;
;; Procedures for manipulating sums:
;;
(defun sum? (x)
  (and (listp x) (eq (car x) '+)))

(defun addend (s) (cadr s))

(defun augend (s) (caddr s))

(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t
	 (list '+ a1 a2))))

(make-sum 0 10)
;; ==> 10
(make-sum 0 'x)
;; ==> x
(make-sum 10 0)
;; ==> 10
(make-sum 'x 0)
;; ==> x 
(make-sum 10 20)
;; ==> 30
(make-sum 'x 'y)
;; ==> (+ x y)
(make-sum 10 'x)
;; ==> (+ 10 x)
(make-sum 'x 10)
;; ==> (+ x 10)

;;
;; Procedures for manipulating products:
;;
(defun product? (x)
  (and (listp x) (eq (car x) '*)))