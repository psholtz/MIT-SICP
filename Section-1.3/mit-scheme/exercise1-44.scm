
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
;; Definition of "smooth-n-times":
;;
(define (smooth-n-times f n)
  ((repeated smooth n) f))
