;;
;; Exercise 2.83
;;
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
  (let ((kind (type-tag n)))
    (cond ((equal? kind 'integer)
	   (make-rational (contents n) 1))
	  ((equal? kind 'scheme-number)
	   (make-rational (truncate n) 1))
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
    (cond ((equal? kind 'scheme-number)
	   (make-complex-from-real-imag n 0))
	  ((equal? kind 'integer)
	   (make-complex-from-real-imag (contents n) 0))
	  (else
	   (error "Argument not scheme-number: " (type-tag n))))))

;;
;; There is a "close kinship" between integer types and real (i.e., scheme-number)
;; types, a kinship which has tempted us into introducing cycles in the coercion
;; graph earlier in the design process above. So as to simplify things from an end-user
;; perspective, we've designed the deigned the raising procedures so that even if they 
;; are "misused" and "misapplied" by the end-user (i.e., the end-user swaps interchanges
;; between integers and scheme-numbers), the procedures should still work as advertised.
;;
;; Accordingly, we check on two types: integer and scheme-number, in the "integer raising"
;; procedure and the "scheme-number" raising procedure.
;;

;;
;; Run through some unit tests:
;;
(raise-integer->rational (make-integer 2))
;; ==> (rational 2 . 1)
(raise-integer->rational (make-integer 2.1))  ;; <-- technically misuing the integer API, but handle gracefully
;; ==> (rational 2. . 1.)
(raise-integer->rational (make-integer 2.6))  ;; <-- technically misuing the integer API, but handle gracefully
;; ==> (rational 2. . 1.)

;;
;; The next two examples are technically "misusing" the API,
;; but so as to guard against "I/O" errors, let's put them here:
;;
(raise-integer->rational 3)    ;; <-- API misuse, but handle gracefully
;; ==> (rational 3 . 1)
(raise-integer->rational 3.1)  ;; <-- API misuse, but handle gracefully
;; ==> (rational 3. . 1)

;;
;; Rational examples:
;;
(raise-rational->scheme-number (make-rational 1 2))
;; ==> 0.5
(raise-rational->scheme-number (make-rational 3 1))
;; ==> 3.

;;
;; Scheme number examples:
;;
(raise-scheme-number->complex 3)
;; ==> (complex rectangular 3 . 0)
(raise-scheme-number->complex 3.14)
;; ==> (complex rectangular 3.14 . 0)

;;
;; Again, the next two examples are technically "misuing" 
;; the API, but they're nice to have:
;;
(raise-scheme-number->complex (make-integer 3))
;; ==> (complex rectangular 3 . 0)
(raise-scheme-number->complex (make-integer 3.14))  ;; <-- technically misuing the integer API, but handle gracefully
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
;; ==>
(raise (make-ratioanl 4 3))
;; ==>
(raise (make-rational 5 1))
;; ==>
(raise (make-rational 10 2))
;; ==>

(raise 3)
;; ==> (complex rectangular 3 . 0)
(raise 3.14)
;; ==> (complex rectangular 3.14 . 0)
(raise (make-scheme-number 3))
;; ==> (complex rectangular 3 . 0)
(raise (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0)