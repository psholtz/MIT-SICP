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