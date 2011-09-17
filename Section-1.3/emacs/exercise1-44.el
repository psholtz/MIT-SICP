;;
;; Exercise 1.44
;;
;; The idea of "smoothing" a function is an important concept in signal processing. If f is a function
;; and dx is some small number, then the smoothed version of f is the function whose value at a point x
;; is the average of f(x-dx), f(x), f(x+dx). Write a procedure "smooth" that takes as input a procedure
;; that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to 
;; repeatedly smoth a function (that is, smooth the smoothed function, and so on) to obtain the 
;; n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function
;; using "smooth" and "repeated" from exercise 1.43.
;;

;;
;; Use lexical scoping
;;
(require 'cl)

;;
;; Define the "smooth" procedure:
;;
(defun smooth (f)
  (lexical-let ((foo f))
	       (setq dx 0.00001)
	       (defun average (a b c)
		 (/ (+ a b c) 3.0))
	       (lambda (x)
		 (average 
		  (funcall foo (- x dx))
		  (funcall foo x)
		  (funcall foo (+ x dx))))))

;;
;; Let's run a use case by defining an impluse function.
;; For the use case, we will use an impulse funtion defined 
;; to be 3 at x=0, and 0 everywhere else. 
;;
;; To construct this impulse, we will use a generic function
;; definition which gives an impulse of "value" at x=a.
;;
(defun impulse-maker (a v)
  (lexical-let ((a1 a)
		(v1 v))
	       (lambda (x)
		 (if (= x a1)
		     v1
		   0))))

;;
;; Define our impulse procedure:
;;
(setq impulse (impulse-maker 0 3))

;;
;; Test the impulse:
;;
(funcall impulse -1)
;; ==> 0

(funcall impulse 0)
;; ==> 3

(funcall impulse 1)
;; ==> 0

;;
;; Now let's try to "smooth" the impulse:
;;
(funcall (smooth impulse) 0)
;; ==> 1.0

;;
;; This is what we expect, since (/ (+ 0.0 3.0 0.0) 3.0) evaluates to 1.0
;;
(= (/ (+ 0 3 0) 3.0) (funcall (smooth impulse) 0))     

;;
;; I'm not going to walk through the substitution model call graph, the 
;; way I did for the scheme version.
;;

;;
;; Let's look at a couple more n-fold recursive calls of "smooth":
;;
(funcall (smooth (smooth (smooth impulse))) 0)
;; ==> 0.7777777777777777

(funcall (smooth (smooth (smooth (smooth impulse)))) 0)
;; ==> 0.7037037037037036

(funcall (smooth (smooth (smooth (smooth (smooth impulse))))) 0)
;; ==> 0.6296296296296297

;;
;; Now let's implement the "repeated" abstraction as described in the text.
;;
;; Give definition of "repeated" procedure:
;;
(defun compose (f g)
  (lexical-let ((foo f)
		(goo g))
	       (lambda (x)
		 (funcall foo (funcall goo x)))))

(defun repeated (f n)
  (lexical-let ((foo f))
	       (defun repeated-iter (g c)
		 (lexical-let ((goo g))
			      (cond ((>= c n) goo)
				    (t
				     (repeated-iter (compose goo foo) (+ c 1))))))
	       (repeated-iter foo 1)))

;;
;; Definition of "smooth-n-times":
;;
(defun smooth-n-times (f n)
  (lexical-let ((foo f)
		(n1 n))
	       (funcall (repeated #'smooth n1) foo)))

;;
;; Let's run some unit tests, to see if we get the answers we expect:
;;
(= (funcall (smooth-n-times impulse 1) 0) (funcall (smooth impulse) 0))