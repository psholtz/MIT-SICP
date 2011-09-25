;;
;; Exercise 2.6
;;
;; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in 
;; a language that can manipulate procedures, we can get by without numbers (at least insofar
;; as nonnegative integers are concerned) by implementing 0 and the opereation of adding 1 as
;;
;; (define zero (lambda (f) (lambda (x) x)))
;; (define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x))))) 
;; 
;; This representation is known as "Church numerals" after its inventor, Alonzo Church, the 
;; logician who invented the lambda calculus.
;;
;; Define "one" and "two" directly (not in terms of "zero" and "add-1") (Hint: Use substitution
;; to evaluate (add-1 zero). Give a direct definition of the addition procedure + (not in terms
;; of repeated application of "add-1").
;;
 
;; 
;; We require lexical bindings
;;
(require 'cl)

;;
;; First define the procedures "zero" and "add-1":
;;
(defun zero (f) (lambda (x) x))

(defun add-1 (n)
  (lexical-let ((noo n))
	       (lambda (f)
		 (lexical-let ((foo f))
			      (lambda (x)
				(funcall foo (funcall (funcall noo #'foo) x)))))))

;;
;; To get a sense for what's happening here, let's experiment a bit with these procedures and 
;; see where it gets us. Looking at the code, both procedures appear to take procedures as 
;; arguments, although in the case of "zero" it isn't obvious that this means anything since
;; no matter what argument we pass to zero, it returns an identity procedure in one argument.
;;
;; "add-1" encapsulates a more complicated behavior, and here the argument "n" to "add-1" is 
;; clearly a procedure in one argument.
;;
;; Let's work with the "inc" and "square" procedures, which are both procedures in one argument.
;;
(defun inc (n) (+ n 1))
(defun square (n) (* n n))

;;
;; Applying "zero" to a procedure returns a procedure in one argument, which itself is simply 
;; the identity procedure:
;;
(funcall (zero #'inc) 1)
;; ==> 1
(funcall (zero #'inc) 5)
;; ==> 5
(funcall (zero #'inc) 10)
;; ==> 10
(funcall (zero #'inc) 100)
;; ==> 100

;;
;; Note that this is the case, regardless of which procedure we apply "zero" to:
;;
(funcall (zero #'square) 1)
;; ==> 1
(funcall (zero #'square) 5)
;; ==> 5
(funcall (zero #'square) 10)
;; ==> 10
(funcall (zero #'square) 100)
;; ==> 100

;; 
;; So no matter what procedure we apply "zero" to, it returns a procedure which 
;; itself doesn't really do anything. "zero" is, perhaps, an apt name for this procedure.
;;

;;
;; Applying "zero" to "add-1" will return a procedure in one argument:
;;
(add-1 #'zero)
;; ==> [compound procedure]

;;
;; Judging from the source code for "add-1", the procedure returned consumes a procedure in 
;; one argument, which itself returns a third procedure in one argument. Let's try to apply 
;; (add-1 zero) to one-argument procedures like "inc" and "square", and see what happens:
;;
(funcall (add-1 #'zero) #'inc)
;; ==> [compound procedure]

;;
;; Again a procedure in one argument is returned. 
;;
;; Let's apply this procedure to various numbers:
;;
(funcall (funcall (add-1 #'zero) #'inc) 1)
;; ==> 2
(funcall (funcall (add-1 #'zero) #'inc) 5)
;; ==> 6
(funcall (funcall (add-1 #'zero) #'inc) 10)
;; ==> 11
(funcall (funcall (add-1 #'zero) #'inc) 100)
;; ==> 101

;;
;; Clearly, by "add-1"-ing to "zero", we end up applying the "inc" procedure just once.
;;
;; Let's see what happens if we apply the generated compound procedure to "square":
;;
