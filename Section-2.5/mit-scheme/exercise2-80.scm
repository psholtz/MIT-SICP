;;
;; Exercise 2.80
;;
;; [WORKING]
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