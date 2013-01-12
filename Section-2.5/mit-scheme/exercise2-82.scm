;;
;; Exercise 2.82
;;
;; [WORKING]
;;

;; 
;; apply is already set up to handle an arbitrary number of argument.
;; Some operations submitted to "apply" have 1 arg, some have 2.
;;
;;
;; First lets define some arithmetic operations which take an 
;; arbitrary number of arguments, the best procedures to define
;; this on are: add, sub and mul.:
;;

;; invoke the binary, generic "add"

;;
;; Let's think about how to define an n-argument procedure for "add" for scheme-numbers.
;; We can then use this as a prototype for designing prototypes for the other types we're 
;; supporting:
;;
(define (add-n . args)
  (define (add-n-iter working total)
    (if (null? working)
	total
	(add-n-iter (cdr working) (add total (car working)))))
  (add-n-iter args 0))

;;
;; Let's try it out on scheme-numbers:
;;
(add-n)
;; ==> 0
(add-n 1)
;; ==> 1
(add-n 1 2)
;; ==> 3
(add-n 1 2 3)
;; ==> 6
(add-n 1 2 3 4)
;; ==> 10

;;
;; We defining similar procedures for the other types, we should (in principle) only have to 
;; change the type of the "constant" that is used to invoke the recursion. For instance, instead
;; of invoking the recursion as (add-n-iter args 0), we would use rather (add-n-iter args (make-rational 0 1)), 
;; and so on.
;;

;;
;; We can define an n-argument procedure for "mul" for scheme-numbers in a similar fashion:
;;
(define (mul-n . args)
  (define (mul-n-iter working total)
    (if (null? working)
	total
	(mul-n-iter (cdr working) (mul total (car working)))))
  (mul-n-iter args 1))

;;
;; Running the unit tests:
;;
(mul-n)
;; ==> 1
(mul-n 1)
;; ==> 1
(mul-n 1 2)
;; ==> 2
(mul-n 1 2 3)
;; ==> 6
(mul-n 1 2 3 4)
;; ==> 24

;;
;; Subtraction requires a bit more thought, but can be implemented relatively easily as well:
;;
(define (sub-n . args)
  (define (sub-n-iter working total)
    (if (null? working)
	total
	(sub-n-iter (cdr working) (sub total (car working)))))

  (let ((size (length args)))
    (cond ((= size 0) 0)
	  ((= size 1) (car args))
	  (else
	   (sub-n-iter (cdr args) (car args))))))

;;
;; Running the unit tests:
;;
(sub-n)
;; ==> 0
(sub-n 4)
;; ==> 4
(sub-n 4 3)
;; ==> 1
(sub-n 4 3 2)
;; ==> -1
(sub-n 4 3 2 1)
;; ==> -2

;;
;; Let's install type-specific implementations of each of these procedures into each
;; of the number packages we support.
;;
;; The "new" scheme-number package will look like:
;;
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'add-n '(scheme-number)
       (lambda (. args)
	 
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (p) (= p 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done) 



				       