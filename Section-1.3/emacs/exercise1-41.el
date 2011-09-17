;;
;; Exercise 1.41
;;
;; Define a procedure "double" that takes a procedure of one argument as argument and returns a 
;; procedure that applies the original procedure twice. For example, if "inc" is a procedure that 
;; adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned 
;; by:
;;
;; (((double (double double)) inc) 5)
;;

;;
;; Use lexical scoping
;;
(require 'cl)

;;
;; Definition of double:
;;
(defun double (g)
  (lexical-let ((goo g))
	       (lambda (x) (funcall goo (funcall goo x)))))

(defun inc (n) (+ n 1))

;;
;; I'm not going to walk through the substitution model call graph 
;; for (((double (double double)) inc) 5) the way I did for the 
;; Scheme solution, but the basic process is similar.
;;
;; Note that here we have to "rephrase" the expression a bit, to 
;; get emacs to evaluate it:
;;
(funcall (funcall (double (double #'double)) #'inc) 5)
;; ==> 21

;; 
;; We could as well define the following procedures:
;;
(setq add-two (double #'inc))
(setq add-four (funcall (double #'double) #'inc))
(setq add-sixteen (funcall (double (double #'double)) #'inc))

;;
;; Note that these last two procedures could be rewritten as:
;;
(setq add-four (double (double #'inc)))
(setq add-sixteen (double (double (double (double #'inc)))))

;;
;; So far, we have deduced that (expressed in scheme)
;;
;; (double inc) ==> adds 2
;; ((double double) inc) ==> adds 4 = (2^2)
;; ((double (double double)) inc) ==> adds 16 = ((2^2)^2)
;;
;; From this, we might surmise that:
;; ((double (double (double double))) inc) ==> adds 256 = (((2^2)^2)^2)
;;
;; Typing this into the interpreter, we obtain the expected expression:
;;
(funcall (funcall (double (double (double #'double))) #'inc) 5)