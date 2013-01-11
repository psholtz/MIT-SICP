;; 
;; Exercise 2.83
;;
;; Suppose you are designing a generic arithmetic system for dealing with ...
;; [WORKING]
;;

;;
;; Both the tower diagram and the problem statement refer to a type hierarchy that
;; looks something like the following:
;;
;;  INTEGER --> RATIONAL --> REAL --> COMPLEX
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
  ;; Check to make sure its an integer, otherwise truncate:
  ;;
  (put 'make 'integer
       (lambda (x) (tag (if (integer? x) x (truncate x)))))

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
(make-integer 2.1)     ;; <-- technically misusing the integer API, but handle gracefully
;; ==> (integer . 2.)
(make-integer 2.6)     ;; <-- technically misusing the integer API, but handle gracefully
;; ==> (integer . 2.)

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
  (make-rational n 1))

(define (raise-rational->scheme-number r)
  (let ((n (car r))
	(d (cdr r)))
    (make-scheme-number (exact->inexact (/ n d)))))

(define (raise-scheme-number->complex n)
  (make-complex-from-real-imag n 0))

;;
;; Tracing the call graph through "apply-generic", we see that the 
;; arguments to each of these procedures is not the tagged type 
;; per se, but rather the contents of the tagged type. 
;;
;; So for instance, the argument to "raise-rational->scheme-number" 
;; is not really, e.g., (rational 1 . 2), but only the "contents"
;; of this data structure, e.g., (1 . 2).
;;
;; This same reasoning applies to all the "raise" procedures.
;;

;;
;; Run through some unit tests:
;;
(raise-integer->rational (contents (make-integer 2)))
;; ==> (rational 2 . 1)
(raise-integer->rational (contents (make-integer 2.1)))  ;; <-- technically misuing integer API, but handle gracefully
;; ==> (rational 2. . 1.)
(raise-integer->rational (contents (make-integer 2.6)))  ;; <-- technically misuing integer API, but handle gracefully
;; ==> (rational 2. . 1.)

;;
;; Rational examples:
;;
(raise-rational->scheme-number (contents (make-rational 1 2)))
;; ==> 0.5
(raise-rational->scheme-number (contents (make-rational 3 1)))
;; ==> 3.
(raise-rational->scheme-number (contents (make-rational 6 2)))
;; ==> 3.

;;
;; Scheme number examples:
;;
(raise-scheme-number->complex 3)
;; ==> (complex rectangular 3 . 0)
(raise-scheme-number->complex 3.14)
;; ==> (complex rectangular 3.14 . 0)
(raise-scheme-number->complex (make-scheme-number 3))
;; ==> (complex rectangular 3 . 0)
(raise-scheme-number->complex (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0)
(raise-scheme-number->complex (contents (make-scheme-number 3)))    ;; <-- models how call is really made
;; ==> (complex rectangular 3 . 0) 
(raise-scheme-number->complex (contents (make-scheme-number 3.14))) ;; <-- models how call is really made
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

;;
;; Let's unit test the final generic "raise" procedure:
;;
(raise (make-integer 2))
;; ==> (rational 2 . 1)
(raise (make-integer 3))
;; ==> (rational 3 . 1)
(raise (make-integer 3.14)) ;; <-- technically misuing the integer API, but handle gracefully
;; ==> (rational 3. . 1)

(raise (make-rational 1 2))
;; ==> 0.5
(raise (make-ratioanl 4 3))
;; ==> 1.33333333333
(raise (make-rational 5 1))
;; ==> 5.
(raise (make-rational 10 2))
;; ==> 5.
 
(raise 3)
;; ==> (complex rectangular 3 . 0)
(raise 3.14)
;; ==> (complex rectangular 3.14 . 0)
(raise (make-scheme-number 3))
;; ==> (complex rectangular 3 . 0)
(raise (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0)