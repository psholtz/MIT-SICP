;;
;; Exercise 1.46
;;
;; Several of the numerical methods described in this chapter are instances of an extremely
;; general computational strategy known as iterative improvement. Iterative improvement says that, 
;; to compute something, we start with an initial guess for the answer, test if the guess 
;; is good enough, and otherwise improve the guess and continue the process using the improved
;; gues as the new guess. Write a procedure "iterative-improve" that takes two procedures as 
;; arguments: a method for telling whether a guess is good enough and a method for improving
;; a guess. "Iterative-improve" should return as its value a procedure that takes a guess
;; as argument and keeps improving the guess until it is good enough. Rewrite the "sqrt" procedure
;; of section 1.1.7 and the "fixed-point" procedure of section 1.3.3 in terms of "iterative-improve".
;;

;;
;; The trick is that we have to be able to "iterate" calls to the procedure 
;; until we arrive at an answer that is "good enough". For this reason, we 
;; define an "inner" procedure that we can invoke repeatedly until we arrive
;; at a "good enough" answer.
;;
(define (iterative-improvement good-enough? improve)
  (lambda (first-guess)
    (define (iteration guess)
      (if (good-enough? guess)
	  guess
	  (iteration (improve guess))))
    (iteration first-guess)))

;;
;; Define "sqrt" in terms of "iterative-improvement":
;;
(define (sqrt x)
  (define tolerance 0.00001)
  (define (average x y) (/ (+ x y) 2.0))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improvement good-enough? improve) 1.0))

;;
;; Run some unit tests:
;;
(sqrt 2.0)
(sqrt 3.0)
(sqrt 5.0)
(sqrt 10.0)

;;
;; Define the "fixed-point" procedure in terms of "iterative-improvement":
;;
(define (fixed-point f x)
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (next x) (f x))
  ((iterative-improvement close-enough? next) 1.0))

;;
;; Run some unit tests:
;;
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (average x y) (/ (+ x y) 2.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))


