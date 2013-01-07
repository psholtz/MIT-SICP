;;
;; Exercise 2.83
;;
;; [WORKING]
;;

;;
;; Both the tower diagram and the problem statement refer to a type hierarchy 
;; something like the following:
;;
;;  integer --> rational --> real --> complex
;;
;; We do not presently have an "integer" or "real" number package. Indeed, the 
;; "scheme-number" package could be used as either, perhaps more appropriately 
;; as "real" (since it handles reals), but to simply "substitute" the type 
;; "scheme-number" in the above tower at two places would introduce ungainly 
;; complications and cycles in our tower graph.
;;
;; Let's instead simply agree to call the our present scheme-number package 
;; as our "real" type, and we'll define a new "integer" package which 
;; supports integer operations. Not that the division operation is not supported, 
;; since integers in general are not closed under division.
;;
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))

;;
;; We will also define a coercion procedure, to coerce the integers into 
;; scheme-numbers, should this be required:
;;
(define (integer->scheme-number n)
  (make-scheme-number n))

(put-coercion 'integer 'scheme-number integer->scheme-number)


;;
;; However, we have not hitherto generated "integer" and "real" packages, and, 
;; on the contrary, both "integer" and "real" types are handled uniformly by 
;; our "scheme-number" package. 
;;
;; Accordingly, we will define two hierarchies that looks something more like:
;;
;;  scheme-number ---> rational ---> complex
;;
;;
;; [WORKING --> rewrite this part]
;;
;; We will moreover define "helper" procedures that raise a scheme-number 
;; directly to a complex (if the scheme number is not a rational number), and 
;; which "lower" a rational number to a scheme-number (i.e., convert the 
;; rational representation to a real-number representation as a scheme-number).
;;

;; The scheme number will be our "real".


(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y)
;;
;; Let's also coerce integers into being scheme-numbers:
;;


;;
;; This method uses the "exact-integer?" procedure which is built into MIT Scheme.
;;
(define (raise-scheme-number->rational n)
  ;; Helper method (go straight to complex)
  (define (raise-scheme-number->complex n)
    (make-complex-from-real-imag n 0.0))

  ;; Raise depending on the type
  (if (exact-integer? n)
      (make-rational n 1)
      (raise-scheme-number->complex n)))

;;
;; Raise a rational to complex by first converting it to a real 
;; (i.e., scheme-number):
;;
(define (raise-rational->complex r)
  ;; Helper method (determines if the argument is of type rational)
  (define (is-rational? x)
    (if (pair? x)
	(let ((kind (car x)))
	  (eq? kind 'rational))
	#f))

  ;; Helper method (lower the rational down to real/scheme-number)
  (define (lower-rational->scheme-number x)
    (let ((n (numer x))
	  (d (denom x)))
      (make-scheme-number (/ (* 1.0 n) (* 1.0 d)))))

  ;; Raise to complex
  (if (is-rational? r)
      (let ((x (lower-rational->scheme-number r)))
	(make-complex-from-real-imag x 0.0))
      ;; default, just return the value
      r))

;;
;; Run some unit tests:
;;
(raise-scheme-number->rational (make-scheme-number 3))
;; ==> (rational 3 . 1)
(raise-scheme-number->rational (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0.)

(raise-rational->complex (make-rational 3 1))
;; ==> (complex rectangular 3. . 0.)
(raise-rational->complex (make-rational 5 4))
;; ==> (complex rectangular 1.25 . 0.)

;;
;; We define the generic "raise" procedure:
;;
(define (raise x) (apply-generic 'raise x))

;;
;; Update the operations table:
;;
(put 'raise '(scheme-number) raise-scheme-number->rational)
(put 'raise '(rational)
     (lambda (z) (attach-tag 'complex (raise-rational->complex z))))
    