;;
;; Exercise 2.79
;;
;; [WORKGIN]
;;

;; =================
;; Generic Procedure
;; =================
(define (equ? x y) (apply-generic 'equ? x y))

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
  
  ;; =============
  ;; New Procedure
  ;; ============= 
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  
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

  ;; ============= 
  ;; New Procedure
  ;; ============= 
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (and (= (numer x) (numer y))
	      (= (denom x) (denom y)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

;; New rectangular package:
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
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))

  ;; =============
  ;; New Procedure
  ;; =============
  (put 'equ? '(rectangular)
       (lambda x y) (and (= (real-part x) (real-part y))
			 (= (imag-part x) (imag-part y))))

  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; New polar package:
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
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))

  ;; =============
  ;; New Procedure
  ;; =============
  (put 'equ? '(polar) (lambda (x y)
			(let ((PI 3.141592653589793238462))
			  (let ((kx (floor (/ (angle x) (* 2 PI))))
				(ky (floor (/ (angle y) (* 2 PI)))))
			    (and (= (magnitude x) (magnitude y))
				 (= (- (angle x) (* kx 2 PI))
				    (- (angle y) (* ky 2 PI))))))))

  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;
;; Reinstall the number packages:
;;
(install-scheme-number-package)
(install-rational-package)
