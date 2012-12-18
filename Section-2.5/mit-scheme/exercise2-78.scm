;;
;; Exercise 2.78
;;
;; [working]
;;

;;
;; We give the original and modified versions of the procedures.
;;
;; The modified procedures check whether the datum is a number, and respond accordingly.
;;

;; Original:
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;; Modified:
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; Original:
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
;; Modified:
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else
	 (error "Bad tagged datum -- TYPE-TAG" datum))))

;; Original:
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;; Modified:
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

;;
;; Unit tests:
;;
(define d1 (attach-tag 'label 'value))
;; ==> (label . value)
(define d2 (attach-tag 'scheme-number 1))
;; ==> 1
(define d3 (attach-tag 'scheme-nubmer 0.5))
;; ==> 0.5

(type-tag d1)
;; ==> label
(type-tag d2)
;; ==> scheme-number
(type-tag d3)
;; ==> scheme-number

(contents d1)
;; ==> value
(contents d2)
;; ==> 1
(contents d3)
;; ==> 0.5

;;
;; Let's test the generic arithmetic package:
;;
(define s1 (make-scheme-number 1))
;; ==> 1
(define s2 (make-scheme-number 2))
;; ==> 2

(add s1 s2)
;; ==> 3 
(sub s1 s2)
;; ==> -1
(mul s1 s2)
;; ==> 2
(div s1 s2)
;; ==> 1/2