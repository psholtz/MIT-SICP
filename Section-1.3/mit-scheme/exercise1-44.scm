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

((smooth (smooth impulse)) 0)
;; ==> 1.0

((smooth (smooth (smooth impulse))) 0)
;; ==> 

((smooth (smooth (smooth (smooth impulse)))) 0)
;; ==>

(= (/ (+ 0 1 0) 3.0) ((smooth impulse) 0))


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
;; +++++ 
;; HERE IS THE CODE
;;
((smooth (smooth impulse)) 0)

((smooth
  (lambda (x)
    (average
     (impulse (- x dx))
     (impulse x)
     (impulse (+ x dx))))) 0)

((lambda (y)
   ((average
     (lambda (x)
       (average 
	(impulse (- x dx))
	(impulse x)
	(impulse (+ x dx)))