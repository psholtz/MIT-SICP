;;
;; Exercise 2.80
;;
;; Define a generic predicate =zero? that tests if its argument is zero, and install 
;; it in the generic arithmetic package. This operation should work for ordinary numbers, 
;; rational numbers, and complex numbers.
;;

;; =================
;; Generic Procedure
;; ================= 
(define (=zero? p) (apply-generic '=zero? p))

;; New scheme number package:
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  
  ;; ============= 
  ;; New Procedure
  ;; =============
  (put '=zero? '(scheme-number)
       (lambda (p) (= p 0)))
  
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done) 

;; New rational package:
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		   (* (numer y) (denom x)))
	                  (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		   (* (numer y) (denom x)))
	                  (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	                  (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	                  (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (and (= (numer x) (numer y))
	      (= (denom x) (denom y)))))
  
  ;; ============= 
  ;; New Procedure
  ;; =============
  (put '=zero? '(rational)
       (lambda (p) (= (numer p) 0)))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

;;
;; For the complex package, we will handle the zero
;; predicate differently for the rectangular and polar
;; cases. In the rectangular case, we will verify that 
;; both the real and imag components are zero. For the 
;; polar case, we will simply check that the magnitude 
;; is zero. This perhaps "cleaner" than checking that 
;; real and imag are zero even on polar numbers.
;;

;; New rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	          (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  ;; ============= 
  ;; New Procedure
  ;; ============= 
  (put '=zero? '(rectangular)
       (lambda (p)
	 (and (= 0 (real-part p))
	      (= 0 (imag-part p)))))

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; New polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	    (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)

  ;; ============= 
  ;; New Procedure
  ;; ============= 
  (put '=zero? '(polar)
       (lambda (p)
	 (= 0 (magnitude p))))
  
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; New complex package
(define (install-complex-package)
  ;; constructors
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			   (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			   (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		                     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		                     (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y)
	 (define (diff a b) (abs (- a b)))
	 (let ((tolerance 0.0000001))
	   (and (< (diff (real-part x) (real-part y)) tolerance)
		(< (diff (imag-part x) (imag-part y)) tolerance)))))
  
  ;; ============= 
  ;; New Procedure
  ;; =============
  (put '=zero? '(complex) =zero?)

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

;;
;; Reinstall the number packages:
;;
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;;
;; Run some unit tests:
;;
(=zero? (make-scheme-number 1))
;; ==> #f
(=zero? (make-scheme-number 0))
;; ==> #t
(=zero? (add (make-scheme-number 1) (make-scheme-number -1)))
;; ==> #t
(=zero? (sub (make-scheme-number 1) (make-scheme-number 1)))
;; ==> #t

(=zero? (make-rational 1 2))
;; ==> #f
(=zero? (make-rational 0 2))
;; ==> #t
(=zero? (add (make-rational 1 2) (make-rational -1 2)))
;; ==> #t
(=zero? (sub (make-rational 1 2) (make-rational 1 2)))
;; ==> #t

(=zero? (make-complex-from-real-imag 1 2))
;; ==> #f
(=zero? (make-complex-from-real-imag 0 1))
;; ==> #f
(=zero? (make-complex-from-real-imag 1 0))
;; ==> #f
(=zero? (make-complex-from-real-imag 0 0))
;; ==> #t

(=zero? (add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag -1 -2)))
;; ==> #f
(=zero? (sub (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)))
;; ==> #t

(=zero? (make-complex-from-mag-ang 1 2))
;; ==> #f
(=zero? (make-complex-from-mag-ang 1 0))
;; ==> #f
(=zero? (make-complex-from-mag-ang 0 1))
;; ==> #t

(=zero? (add (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang -1 -2)))
;; ==> #f
(=zero? (sub (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2)))
;; ==> #t

(=zero? (mul (make-complex-from-real-imag 1 2) (make-complex-from-mag-ang 0 1)))
;; ==> #t
