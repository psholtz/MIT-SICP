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
;; Let's first use the substitution model to simply the expression for ((double (double double)) inc):
;;

((double (double double)) inc)
((double (lambda (x) (double (double x)))) inc) 
((lambda (y) ((lambda (x) (double (double x))) ((lambda (x) (double (double x))) y))) inc) 
((lambda (x) (double (double x))) ((lambda (x) (double (double x))) inc)) 
((lambda (x) (double (double x))) (double (double inc))) 
(double (double (double (double inc)))) 

;;
;; So we're obtained a simplified expression for ((double (double double)) inc). 
;;
;; Semantically, it would appear that we're "doubling" application of the expression four 
;; times. If "inc" simply adds 1 to its argument expression, we might expect that 
;; (double (double (double (double inc)))) would behave something like 2^4 (that is, 4 doubles).
;;
;; Let's investigate this by simplifying the expression further:
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
;; Looks formidable! 

(((double (double double)) inc) 5)
(((double (lambda (x) (double (double x)))) inc) 5)
(((lambda (y) (lambda (x) (double (double x))) (lambda (x) (double (double x))) y)
(double inc)
(lambda (x) (inc (inc x)))

;;
;; So (double inc) produces a one-argument procedure that applies inc twice. 
;;
;; We can write:
;;
(define add-two (double inc))

;;
;; Now let's see what (double double) evaluates to:
;;
(double double)
(lambda (x) (double (double x)))


;; (double g) will apply the procedure "g" twice. 
;;
;; So evaluation of (double inc) will produce a procedure that applies "inc" once (adding one), and 
;; then applies "inc" again, adding a total of two. 
;;
;; We can write:
;;
(define add-two (double inc))


;; (double double) will apply the argument procedure twice times two (or four times total). 
;; That is, if "inc" is a procedure that increments by 1, ((double double) inc) will produce 
;; a procedure that increments by 4.
;;
;; Similarly, (double (double double)) will apply the argument procedure twice times two times two, 
;; or eight times total. ((double (double double)) inc) will produce a procedure that increments 
;; by 8.
;;
(((double (double double)) inc) 5)

