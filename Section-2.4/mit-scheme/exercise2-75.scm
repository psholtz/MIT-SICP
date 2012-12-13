;;
;; Exercise 2.75
;;
;; Implement the constructor "make-from-mag-ang" in message-passing style. This procedure should be 
;; analogous to the "make-from-real-imag" procedure given above.
;;

(load "complex-package.scm")

;;
;; The definition of "make-from-real-imag" given in the text is as follows:
;;
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude)
	   (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle) (atan y x))
	  (else
	   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;
;; And we also need to change the "apply-generic" procedure:
;;
(define (apply-generic op arg) (arg op))

;; 
;; Let's run some unit tests.
;;
;; One thing we notice right away is that constructing numbers
;; no longer returns an ordinary data structure that we can 
;; easily introspect from the interpreter, but rather returns 
;; a procedure (i.e., closure):
;;
(define test1 (make-from-real-imag 1 2))
;; ==> #[compound procedure]
(real-part test1)
;; ==> 1
(imag-part test1)
;; ==> 2
(magnitude test1)
;; ==> 2.23607
(angle test1)
;; ==> 1.10715

;;
;; For interpreting angles, it's nice to have a radian->degree converter:
;;
(define (radian->degree angle)
  (define PI 3.14159)
  (* (/ angle PI) 180.0))

(radian->degree (angle test1))
;; ==> 63.435

(define test2 (make-from-real-imag 0 1))
;; ==> #[compound procedure]
(real-part test2)
;; ==> 0
(imag-part test2)
;; ==> 1
(magnitude test2)
;; ==> 1
(radian->degree (angle test2))
;; ==> 90.000

(define test3 (add-complex test1 test2))
;; ==> #[compound procedure]
(real-part test3)
;; ==> 1
(imag-part test3)
;; ==> 3

;;
;; Now, define the constructor "make-from-mag-ang" as requested:
;;
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part)
	   (* r (cos a)))
	  ((eq? op 'imag-part)
	   (* r (sin a)))
	  (else 
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;
;; Let's run some unit tests:
;;
(define c1 (make-from-mag-ang 1 0))
;; ==> #[compound procedure]

(real-part c1)
;; ==> 1
(imag-part c1)
;; ==> 0

(define PI 3.14159)
(define c2 (make-from-mag-ang 1 (/ pi 2)))
;; ==> #[compound procedure]

(real-part c2)
;; ==> 0
(imag-part c2)
;; ==> 1

(define c3 (make-from-real-imag 2 1))
;; ==> #[compound procedure]

(magnitude c3)
;; ==> 2.236068
(angle c3)
;; ==> 0.463648

(define c4 (add-complex c1 c2))
;; ==> #[compound procedure]

(real-part c4)
;; ==> 1
(imag-part c4)
;; ==> 1

;;
;; These results correspond with what we obtained earlier.
;;