;;
;; Exercise 1.42
;;
;; Let f and g be two one-argument functions. The "composition" f after g is defined to be the function 
;; x --> f(g(x)). Define a procedure "compose" that implements composition. For example, if "inc is a 
;; procedure that adds 1 to its argument.
;;
;; ((compose square inc) 6)
;; --> 49
;;

;;
;; We need to use lexical-scoping:
;;
(require 'cl)

;; 
;; Definition of the "compose" procedure:
;;
(defun compose (f g)
  (lexical-let ((foo f)
		(goo g))
	       (lambda (x)
		 (funcall foo (funcall goo x)))))

;;
;; Run the sample given in text:
;;
(defun inc (n) (+ n 1))
(defun square (n) (* n n))

(funcall (compose #'square #'inc) 6)
;; ==> 49