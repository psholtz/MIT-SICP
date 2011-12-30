;;
;; Exercise 3.8
;;
;; When we defined the evaluation model in section 1.1.3, we said that the first step 
;; in evaluating an expression is to evaluate its subexpressions. But we never specified
;; the order in which the subexpressions should be evaluated (e.g., left to right or 
;; right to left). When we introduce assignment, the order in which the arguments to 
;; a procedure are evaluated can make a difference to the result. Define a simple procedure 
;; f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated 
;; from left to right but will return 1 if the arguments are evaluated from right to left.
;;

;;
;; The intuition is to define a function that returns its argument the 
;; "first" time it is invoked, and then returns "0" with every other 
;; invocation. Making the invocations (f 1) and (f 0) should generate
;; the results 1 0, while making the invocations (f 0) and (f 1) should
;; generate the results 0 0.
;;

;; 
;; Let's start by definition a procedure "g" that returns the number of 
;; times its been invoked, starting with its argument value "start":
;; (Note that the argument "y" is here a dummy, unused variable)
;;
(define (g start)
  (lambda (y)
    (begin (set! start (+ start 1))
	   start)))

;;
;; Let's define another procedure "h", that starts counting (using "g") at 0:
;;
(define h (g 0))

;;
;; Finally, let's define a procedure "f" that returns its argument the first 
;; time its invoked, and returns 0 for each subsequence invocation:
;;
(define (f x)
  (if (= (h x) 1)
      x
      0))

;;
;; Testing this out:
;;
(f 100)
;; ==> 100
(f 124)
;; ==> 0
(f 1002)
;; ==> 0

;;
;; Seems to work..
;;

;;
;; We can test the order in which the interpreter evaluates arguments by 
;; evaluating the expression defined in the text:
;;
(+ (f 0) (f 1))
;; ==> 1

;;
;; It would seem that MIT Scheme evaluates the arguments from right to left.
;;

;;
;; We can model the "reverse" evaluation procedure by switching the order of
;; the arguments in the expression:
;;
(+ (f 1) (f 0))
;; ==> 0