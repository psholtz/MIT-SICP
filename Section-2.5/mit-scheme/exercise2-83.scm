;;
;; Exercise 2.83
;;
;; [WORKING]
;;

;;
;; Both the tower diagram and the problem statement refer to a type hierarchy that
;; looks something like the following:
;;
;;  integer --> rational --> real --> complex
;;
;; We do not presently have an "integer" or a "real" number package. 
;; 
;; Let's agree to use our pre-existing "scheme-number" package as the "real" package.
;;
;; Let's furthermore design an "integer" package which will be very similar to 
;; the "scheme-number" package, except that the domain will be restricted to integers.
;; Notably, we will not support the division operation, since integers are not closed
;; under the division operation.
;; 
(define (install-integer-package)
  ;;
  ;; We have to attach "real" tags for integer, otherwise integers are indistinguishable 
  ;; from scheme-numbers (i.e., reals), and we get a cycle in our conversion graph:
  ;;
  (define (tag x)
    (cons 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (p) (= p 0)))
  ;;
  ;; Check to make sure its an integer, otherwise round to nearest integer:
  ;;
  (put 'make 'integer
       (lambda (x) (tag (if (integer? x) x (round x)))))

  'done)

(define (make-integer n)
  ((get 'make 'integer) n))
  
;;
;; Install the integer package:
;;
(install-integer-package)

;;
;; Let's run some unit tests:
;;
(define i1 (make-integer 2))
;; ==> (integer . 2)
(define i2 (make-integer 3))
;; ==> (integer . 3)
(define i3 (make-integer 0))
;; ==> (integer . 0)

(add i1 i2)
;; ==> (integer . 5)
(sub i1 i2)
;; ==> (integer . -1)
(mul i1 i2)
;; ==> (integer . 6)
(exp i1 i2)
;; ==> (integer . 8)
(equ? i1 i2)
;; ==> #f
(equ? i1 i1)
;; ==> #t
(=zero? i1)
;; ==> #f
(=zero? i3)
;; ==> #t

;;
;; Let's see if we only get "integers" when constructing integers:
;;
(make-integer 2)
;; ==> (integer . 2)
(make-integer 2.1)
;; ==> (integer . 2.)
(make-integer 2.6)
;; ==> (integer . 3.)

;;
;; Note that we cannot divide integers (as is appropriate), since integers are 
;; not closed under division, and we have not defined a "div" procedure for types
;; '(integer integer):
;;
(div i1 i2)
;; ==> No method for these types: (div (integer integer))

;;
;; Neither can we combine integers and scheme-numbers (i.e., reals) as you 
;; might expect, since we haven't defined coercion for these types:
;;
(define s1 (make-scheme-number 2))

(add i1 s1)
;; ==> No method for these types: (add (integer scheme-number))
(sub i1 a1)
;; ==> No method for these types: (sub (integer scheme-number))
(mul i1 s1)
;; ==> No method for these types: (mul (integer scheme-number))
(exp i1 s1)
;; ==> No method for these types: (exp (integer scheme-number))

;;
;; Now let's define our "raise" procedures:
;;
(define (raise-integer->rational n)
  (let ((kind (type-tag n)))
    (cond ((equal? kind 'integer)
	   (make-rational (contents n) 1))
	  (else
	   (error "Argument not an integer: " (type-tag n))))))

(define (raise-rational->scheme-number r)
  (let ((kind (type-tag r)))
    (cond ((equal? kind 'rational)
	   (let ((n (numer r))
		 (d (denom r)))
	     (make-scheme-number (exact->inexact (/ n d)))))
	  (error "Argument not rational: " (type-tag r)))))
    
(define (raise-scheme-number->complex n)
  (let ((kind (type-tag n)))
    (if (equal? kind 'scheme-number)
	(make-complex-from-real-imag n 0)
	(error "Argument not scheme-number: " (type-tag n)))))

;;
;; Run through some unit tests:
;;
(raise-integer->rational (make-integer 2))
;; ==> (rational 2 . 1)
(raise-integer->rational (make-integer 2.1))
;; ==> (rational 2. . 1.)
(raise-integer->rational (make-integer 2.6))
;; ==> (rational 3. . 1.)

(raise-rational->scheme-number (make-rational 1 2))
;; ==> 0.5
(raise-rational->scheme-number (make-rational 3 1))
;; ==> 3.

(raise-scheme-number->complex 3)
;; ==> (complex rectangular 3 . 0)
(raise-scheme-number->complex 3.14)
;; ==> (complex rectangular 3.14 . 0)

;;
;; We now define the generic "raise" procedure:
;;
(define (raise x) (apply-generic 'raise x))

;;
;; And we update the operations table:
;;
(put 'raise '(integer) raise-integer->rational)
(put 'raise '(rational) raise-rational->scheme-number)
(put 'raise '(scheme-number) raise-scheme-number->complex)

    