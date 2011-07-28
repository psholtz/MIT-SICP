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
;; Evaluation of (((double (double double)) inc) 5)
;; ------------------------------------------------ 
;;
;; Let's first use the substitution model to simplify the expression for ((double (double double)) inc):
;;
((double (double double)) inc)
((double (lambda (x) (double (double x)))) inc) 
((lambda (y) ((lambda (x) (double (double x))) ((lambda (x) (double (double x))) y))) inc) 
((lambda (x) (double (double x))) ((lambda (x) (double (double x))) inc)) 
((lambda (x) (double (double x))) (double (double inc))) 
(double (double (double (double inc)))) 

;;
;; So we've obtained a simplified expression for ((double (double double)) inc). 
;;
;; Semantically, it would appear that we're "doubling" application of the expression four 
;; times. If "inc" simply adds 1 to its argument expression, we might expect that 
;; (double (double (double (double inc)))) would behave something like adding 2^4 (that is, 4 doubles).
;;
;; Let's investigate this possibility by simplifying the expression further:
;;
(double (double (double (double inc))))

(double (double (double (lambda (x) (inc (inc x))))))

(double (double (lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))))

(double (lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) 
		     ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))))

(lambda (u) ((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
			  ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) 
	     ((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
			   ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) u)))

;;
;; Looks formidable! But it's still just a function of one variable. Let's apply it to 5:
;;
((lambda (u) ((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
			  ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) 
	     ((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
			   ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) u))) 5)

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	       ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) 5))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) 5)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) 5))))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  ((lambda (x) (inc (inc x))) (inc (inc 5)))))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  ((lambda (x) (inc (inc x))) (inc 6))))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  ((lambda (x) (inc (inc x))) 7)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  (inc (inc 7))))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
  (inc 8)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) 9))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) 9)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (x) (inc (inc x))) (inc (inc 9))))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (x) (inc (inc x))) (inc 10)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 ((lambda (x) (inc (inc x))) 11))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 (inc (inc 11)))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z)))
 (inc 12))

((lambda (z) ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
	      ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) z))) 13)

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 ((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) 13))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) 13)))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 ((lambda (x) (inc (inc x))) (inc (inc 13))))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 ((lambda (x) (inc (inc x))) (inc 14)))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 ((lambda (x) (inc (inc x))) 15))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 (inc (inc 15)))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y)))
 (inc 16))

((lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))) 17)

((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) 17))

((lambda (x) (inc (inc x))) (inc (inc 17)))

((lambda (x) (inc (inc x))) (inc 18))

((lambda (x) (inc (inc x))) 19)

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