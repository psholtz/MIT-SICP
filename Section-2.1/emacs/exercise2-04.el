;;
;; Exercise 2.4
;;
;; Here is an alternative procedural representation of pairs. For this representation, 
;; verify that (car (cons x y)) yields x for any objects x and y.
;;
;; (define (cons x y)
;;  (lambda (m) (m x y)))
;;
;; (define (car z)
;;  (z (lambda (p q) p)))
;;
;; What is the corresponding definition of cdr? 
;;

;;
;; This example is difficult to work in Emacs Lisp. 
;;
;; Specifically, if we naively try to implement "cons" as something like the following:
;;
;; (defun cons (x y)
;;  (lexical-let ((xoo x)
;;                (yoo y))
;;   (lambda (m) 
;;    (lexical-let ((moo m))
;;     (funcall moo xoo yoo)))))
;;
;; and we attempt to use this in emacs, we get a "max recursion depth" error, even when 
;; trying to construct something as simple as (cons 1 2).
;;
;; Apparently in emacs the namespace cannot be manipulating in the way we are trying to do
;;
;; Instead, if we attach unique names to the procedures, we meet with more luck:
;;

;;
;; Use lexical bindings
;;
(require 'cl)

;;
;; Define the custom version of "cons":
;;
(defun my-cons (x y)
  (lexical-let ((xoo x)
		(yoo y))
	       (lambda (m)
		 (lexical-let ((moo m))
			      (funcall moo xoo yoo)))))

;;
;; The definition of the custom version of "car":
;;
(defun my-car (z)
  (lexical-let ((zoo z))
	       (funcall zoo (lambda (p q) p))))

;;
;; I'm not going to expand the code graph in emacs as I did for the scheme version.
;; Instead, I'll simply give the "custom" version of cdr for emacs:
;;
(defun my-cdr (z)
  (lexical-let ((zoo z))
	       (funcall zoo (lambda (p q) q))))