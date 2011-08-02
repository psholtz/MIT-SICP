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
;; Define the "smooth" procedure:
;;
(define (smooth f)
  (define dx 0.00001)       ;; define the "dx" differential
  (define (average a b c)
    (/ (+ a b c) 3.0))
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

;;
;; Let's run a use case by defining an impluse function.
;; For the use case, we will use an impulse funtion defined 
;; to be 3 at x=0, and 0 everywhere else. 
;;
;; To construct this impulse, we will use a generic function
;; definition which gives an impulse of "value" at x=a.
;;
(define (impulse-maker a value)
  (lambda (x)
    (if (= x a)
	value
	0)))

;;
;; Define our impulse procedure:
;;
(define impulse (impulse-maker 0 3))

;;
;; Test the impulse:
;;
(impulse -1)
;; ==> 0

(impulse 0)
;; ==> 3

(impulse 1)
;; ==> 0

;;
;; Now let's try to "smooth" the impulse:
;;
((smooth impulse) 0)
;; ==> 1.0

;;
;; This is what we expect, since (/ (+ 0.0 3.0 0.0) 3.0) evaluates to 1.0
;;
(= (/ (+ 0 3 0) 3.0) ((smooth impulse) 0))

;;
;; Let's examine the call graph for ((smooth impulse) 0) to see how the 
;; interpreter arrives at this answer:
;;
-----------------------------
((smooth impulse) 0)
----------------------------- 
((lambda (x)
   (average 
    (impulse (- x dx))
    (impulse x)
    (impulse (+ x dx)))) 0)
------------------------------ 
(average (impulse (- 0 dx))
	 (impulse 0)
	 (impulse (+ 0 dx)))
------------------------------  
(average 0 3 0)
------------------------------ 
1.0
------------------------------ 

;;
;; Now let's try to compose the "smooth" function with itself,  
;; and see what we get for an answer:
;;
((smooth (smooth impulse)) 0)
;; ==> 1.0

;;
;; This may seem a bit surprising. If ((smooth impulse) 0) is equal 
;; to 1, we might naively assume that ((smooth (smooth impulse)) 0) 
;; should be equal to 1/3 (i.e., cuts the value of (smooth impulse)
;; at zero down again by 1/3). 
;;
;; To see why this is not the case, let's expand the call graph 
;; for ((smooth (smooth impulse)) 0):
;;
----------------------------------
((smooth (smooth impulse)) 0)
---------------------------------- 
((smooth
  (lambda (x)
    (average 
     (impulse (- x dx))
     (impulse x)
     (impulse (+ x dx))))) 0)
----------------------------------
((lambda (y)
   (average 
    ((lambda (x)
       (average
	(impulse (- x dx))
	(impulse x)
	(impulse (+ x dx))))
     (- y dx))
    ((lambda (x)
       (average 
	(impulse (- x dx))
	(impulse x)
	(impulse (+ x dx))))
     y)
    ((lambda (x)
       (average
	(impulse (- x dx))
	(impulse x)
	(impulse (+ x dx))))
     (+ y dx)))) 0)
---------------------------------- 
(average
 ((lambda (x)
    (average
     (impulse (- x dx))
     (impulse x)
     (impulse (+ x dx))))
  (- 0 dx))
 ((lambda (x)
    (average
     (impulse (- x dx))
     (impulse x)
     (impulse (+ x dx))))
  0)
 ((lambda (x)
    (average
     (impulse (- x dx))
     (impulse x)
     (impulse (+ x dx))))
  (+ 0 dx)))
----------------------------------- 
(average
 (average 
  (impulse (- (- 0 dx) dx))
  (impulse (- 0 dx))
  (impulse (+ (- 0 dx) dx)))
 (average
  (impulse (- 0 dx))
  (impulse 0)
  (impluse (+ 0 dx)))
 (average
  (impulse (- (+ 0 dx) dx))
  (impulse (+ 0 dx))
  (impulse (+ (+ 0 dx) dx))))
------------------------------------ 
(average
 (average
  (impulse (- -0.00001 dx))
  (impulse -0.00001)
  (impulse (+ -0.00001 dx)))
 (average 
  (impulse -0.00001)
  (impulse 0)
  (impulse +0.00001))
 (average
  (impulse (- +0.00001 dx))
  (impulse +0.00001)
  (impulse (+ +0.00001 dx))))
------------------------------------- 
(average
 (average
  (impulse -0.00002)
  (impulse -0.00001)
  (impulse 0))
 (average
  (impulse -0.00001)
  (impulse 0)
  (impulse +0.00001))
 (average
  (impulse 0)
  (impulse +0.00001)
  (impulse +0.00002)))
-------------------------------------- 
(average
 (average 0 0 3)
 (average 0 3 0)
 (average 3 0 0))
--------------------------------------
(average 1.0 1.0 1.0)
-------------------------------------- 
1.0
-------------------------------------- 

;;
;; So indeed, ((smooth (smooth impulse)) 0) works out to 1.0 as well.
;;

;;
;; Let's look at a couple more n-fold recursive calls of "smooth":
;;
((smooth (smooth (smooth impulse))) 0)
;; ==> 0.777777777777777

((smooth (smooth (smooth (smooth impulse)))) 0)
;; ==> 0.703703703703703

((smooth (smooth (smooth (smooth (smooth impulse))))) 0)
;; ==> 0.629629629629629

;;
;; Now let's implement the "repeated" abstraction as described in the text.
;;
;; Give definition of "repeated" procedure:
;;
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter g c)
    (cond ((>= c n) g)
	  (else
	   (repeated-iter (compose g f) (+ c 1)))))
  (repeated-iter f 1))

;;
;; Let's run some unit tests to see if "repeated" works the way we

;;
;; Definition of "smooth-n-times":
;;
(define (smooth-n-times f n)
  ((repeated smooth n) f))

;;
;; Let's run some unit tests, to see if we get the answers we expect:
;;
(= ((smooth-n-times impulse 1) 0) ((smooth impulse) 0))
(= ((smooth-n-times impulse 2) 0) ((smooth (smooth impulse)) 0))
(= ((smooth-n-times impulse 3) 0) ((smooth (smooth (smooth impulse))) 0))
(= ((smooth-n-times impulse 4) 0) ((smooth (smooth (smooth (smooth impulse)))) 0))
(= ((smooth-n-times impulse 5) 0) ((smooth (smooth (smooth (smooth (smooth impulse))))) 0))