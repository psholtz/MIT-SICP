;;
;; Exercise 2.12
;;
;; Define a constructor "make-center-percent" that takes a center and a percentage
;; and produces the desired interval. You must also define a selector "percent"
;; that produces the percentage tolerance for a given interval. The "center" selector
;; is the same as the one shown above.
;;

;;
;; First define the original constructor and selectors:
;;
(define (make-interval a b)
  (cond ((< a b) (cons a b))
	(else
	 (display "error constructing interval!"))))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;;
;; Define the "additive tolerance" constructors from the text:
;;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;
;; A use case given in the text is: 3.5 += 0.15, resulting in [3.35, 3.65].
;;
;; Let's see if our code is able to reproduce this:
;;
(define x (make-center-width 3.5 0.15))
(center x)
;; ==> 3.5
(width x)
;; ==> 0.15
(lower-bound x)
;; ==> 3.35
(upper-bound x)
;; ==> 3.65

;;
;; Finally answer the question:
;;
(define (make-center-percent c p)
  '())

;;
;; The "center" selector is already given in the text.
;;
;; We define the "percent" selector:
;;
(define (percent i)
  '())