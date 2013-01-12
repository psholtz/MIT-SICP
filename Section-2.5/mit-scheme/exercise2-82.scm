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
;; supporting.
;;
;; As usual, we will define the recursion in terms of an iterative procedure, but for 
;; reasons that will become apparent shortly, we'll define that iterative procedure 
;; in the global namespace, rather than in the namespace local to the calling function:
;;
(define (add-n-iter working total)
  (if (null? working)
      total
      (add-n-iter (cdr working) (add total (car working)))))

(define (add-n . args)
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
;; When defining similar procedures for the other types, we should (in principle) only have to 
;; change the type of the "constant" that is used to invoke the recursion. For instance, instead
;; of invoking the recursion as (add-n-iter args 0), we would use rather (add-n-iter args (make-rational 0 1)), 
;; and so on.
;;

;;
;; Notice too that "add-n-iter", as defined above, is itself a "generic" procedure in the sense
;; that the only "outside" invocation to a procedure is through "add", which itself is generic.
;; So long as all the arguments in the "args" like (in the "add-n" procedure) are of the same 
;; type, our arithmetic procedure should be able to handle the n-dimensional addition for any 
;; of our supported types.
;;

;;
;; We can define an n-argument procedure for "mul" for scheme-numbers in a similar fashion:
;;
(define (mul-n-iter working total)
  (if (null? working)
      total
      (mul-n-iter (cdr working) (mul total (car working)))))

(define (mul-n . args)
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
;; Each of the *-iter procedures has an extremely similar form, differing only in the operation 
;; that is applied at each iteration. We abstract further as follows:
;;
(define (combination-n-iter op working total)
  (if (null? working)
      total
      (combination-n-iter op (cdr working) (op total (car working)))))

(define (add-n-iter working total)
  (combination-n-iter add working total))
(define (mul-n-iter working total)
  (combination-n-iter mul working total))

;;
;; Running the unit tests on scheme-numbers with these definitions yields the same results as before.
;;

;;
;; But wait! Magically, these definitions of n-dimensional arithmetic operations have been transformed
;; into generic operations, as we intended above; for all we need to do to invoke the correct "form" of
;; add, etc., is to much sure the "initial" value sent to total is of the correct type (and that all 
;; elements in the argument list are match this type):
;;
(add-n-iter (list (make-rational 1 1) (make-rational 1 2)) (make-rational 0 1))
;; ==> (rational 3 . 2)
(mul-n-iter (list (make-rational 1 2) (make-rational 2 3)) (make-rational 1 1))
;; ==> (rational 1 . 3)

(add-n-iter (list (make-complex-from-real-imag 1 1)
		  (make-complex-from-real-imag 2 2))
	    (make-complex-from-real-imag 0 0))
;; ==> (complex rectangular 3 . 3)
(mul-n-iter (list (make-complex-from-real-imag 1 0))
	    (make-complex-from-real-imag 1 0))
;; ==> (complex polar 1 . 0)

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
       (lambda args (add-n-iter args (make-scheme-number 0))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'mul-n '(scheme-number)
       (lambda args (mul-n-iter args (make-scheme-number 1))))
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

;;
;; Finally, let's define the n-dimensional arithmetic procedures "generically". 
;;
(define (add-n . args)
  (if (empty? args)
      0
      (let ((elem (car args)))
	(let ((kind (type-tag elem)))
	  (cond ((eq? kind 'scheme-number)
		 (apply '
  




				       