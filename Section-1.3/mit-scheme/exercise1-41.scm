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
;; Definition of double:
;;
(define (double g)
  (lambda (x) (g (g x))))

;;
;; Evaluation of (((double (double double)) inc) 5):
;;
------------------------------------------------------------------------------------------------
(((double (double double)) inc) 5)
------------------------------------------------------------------------------------------------
(((double (lambda (x) (double (double x)))) inc) 5)
------------------------------------------------------------------------------------------------
(((lambda (y) 
    ((lambda (x) (double (double x)))
     ((lambda (x) (double (double x))) y))) inc) 5)
------------------------------------------------------------------------------------------------
(((lambda (x) (double (double x)))
  ((lambda (x) (double (double x))) inc)) 5)
------------------------------------------------------------------------------------------------
(((lambda (x) (double (double x)))
  (double (double inc))) 5)
------------------------------------------------------------------------------------------------
(((lambda (x) (double (double x)))
  (double (lambda (y) (inc (inc y))))) 5)
------------------------------------------------------------------------------------------------
(((lambda (x) (double (double x)))
  (lambda (z)
    ((lambda (y) (inc (inc y))) 
     ((lambda (y) (inc (inc y))) z)))) 5)
------------------------------------------------------------------------------------------------
((double 
  (double 
   (lambda (z)
     ((lambda (y) (inc (inc y)))
      ((lambda (y) (inc (inc y))) z))))) 5)
-------------------------------------------------------------------------------------------------
((double
  (lambda (u)
    ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
     ((lambda (z)
	((lambda (y) (inc (inc y)))
	 ((lambda (y) (inc (inc y))) z))) u)))) 5)
--------------------------------------------------------------------------------------------------
((lambda (v)
   ((lambda (u)
      ((lambda (z)
	 ((lambda (y) (inc (inc y)))
	  ((lambda (y) (inc (inc y))) z)))
       ((lambda (z)
	  ((lambda (y) (inc (inc y)))
	   ((lambda (y) (inc (inc y))) z))) u)))
    ((lambda (u)
       ((lambda (z)
	  ((lambda (y) (inc (inc y)))
	   ((lambda (y) (inc (inc y))) z)))
	((lambda (z)
	   ((lambda (y) (inc (inc y)))
	    ((lambda (y) (inc (inc y))) z))) u))) v))) 5)
-------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (u)
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z)))
     ((lambda (z)
	((lambda (y) (inc (inc y)))
	 ((lambda (y) (inc (inc y))) z))) u))) 5))
-------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (z)
    ((lambda (y) (inc (inc y)))
     ((lambda (y) (inc (inc y))) z)))
  ((lambda (z)
     ((lambda (y) (inc (inc y)))
      ((lambda (y) (inc (inc y))) z))) 5)))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (z)
    ((lambda (y) (inc (inc y)))
     ((lambda (y) (inc (inc y))) z)))
  ((lambda (y) (inc (inc y)))
   ((lambda (y) (inc (inc y))) 5))))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (z)
    ((lambda (y) (inc (inc y)))
     ((lambda (y) (inc (inc y))) z)))
  ((lambda (y) (inc (inc y)))
   (inc (inc 5)))))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (z)
    ((lambda (y) (inc (inc y)))
     ((lambda (y) (inc (inc y))) z)))
  (inc (inc (inc (inc 5))))))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (y) (inc (inc y)))
  ((lambda (y) (inc (inc y)))
   (inc (inc (inc (inc 5)))))))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 ((lambda (y) (inc (inc y)))
  (inc (inc (inc (inc (inc (inc 5))))))))
--------------------------------------------------------------------------------------------------
((lambda (u)
   ((lambda (z)
      ((lambda (y) (inc (inc y)))
       ((lambda (y) (inc (inc y))) z)))
    ((lambda (z)
       ((lambda (y) (inc (inc y)))
	((lambda (y) (inc (inc y))) z))) u)))
 (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))
---------------------------------------------------------------------------------------------------
((lambda (z)
   ((lambda (y) (inc (inc y)))
    ((lambda (y) (inc (inc y))) z)))
 ((lambda (z)
    ((lambda (y) (inc (inc y)))
     ((lambda (y) (inc (inc y))) z)))
  (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))
--------------------------------------------------------------------------------------------------- 
((lambda (z)
   ((lambda (y) (inc (inc y)))
    ((lambda (y) (inc (inc y))) z)))
 ((lambda (y) (inc (inc y)))
  ((lambda (y) (inc (inc y)))
   (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))
--------------------------------------------------------------------------------------------------- 
((lambda (z)
   ((lambda (y) (inc (inc y)))
    ((lambda (y) (inc (inc y))) z)))
 ((lambda (y) (inc (inc y)))
  (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))
---------------------------------------------------------------------------------------------------
((lambda (z)
   ((lambda (y) (inc (inc y)))
    ((lambda (y) (inc (inc y))) z)))
 (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))
---------------------------------------------------------------------------------------------------
((lambda (y) (inc (inc y)))
 ((lambda (y) (inc (inc y)))
  (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))
---------------------------------------------------------------------------------------------------
((lambda (y) (inc (inc y)))
 (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))
---------------------------------------------------------------------------------------------------
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
---------------------------------------------------------------------------------------------------

;;
;; Hence (((double (double double)) inc) 5) results in 16 calls to "inc", and results
;; in the number 16 being added to the argument. 
;; 
;; The result of the specific evaluation (((double (double double)) inc) 5) is 21.
;; 

;;
;; Let's continue evaluating the expression:
;;

(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 6)))))))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 7))))))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 8)))))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 9))))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 10)))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 11))))))))))
(inc (inc (inc (inc (inc (inc (inc (inc (inc 12)))))))))
(inc (inc (inc (inc (inc (inc (inc (inc 13))))))))
(inc (inc (inc (inc (inc (inc (inc 14)))))))
(inc (inc (inc (inc (inc (inc 15))))))
(inc (inc (inc (inc (inc 16)))))
(inc (inc (inc (inc 17))))
(inc (inc (inc 18)))
(inc (inc 19))
(inc 20)
21

;;
;; Hence, we see that our suspicion was correct, and invoking (double (double (double (double inc))))
;; is like adding 16 to the original number. 
;; 
;; We could as well define the following procedures:
;;
(define add-two (double inc))
(define add-four ((double double) inc))
(define add-sixteen ((double (double double)) inc))

;;
;; Note that these last two procedures could be rewritten as:
;;
(define add-four (double (double inc)))
(define add-sixteen (double (double (double (double inc)))))

;;
;; So far, we have deduced that:
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
(((double (double (double double))) inc) 5)
;; --> 261

