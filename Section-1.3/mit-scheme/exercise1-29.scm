;;
;; Exercise 1.29
;;
;; Simpson's Rule is a more accurate method of numerical integration than the method
;; illustrated above. Using Simpson's Rule, the integral of a function f between 
;; a and b is approximated by:
;;
;; (h/3)*(y0 + 4*y1 + 2*y2 + 4*y3 + 2*y4 + ... + 2*y(n-2) + 4*y(n-1) + y(n))
;;
;; where h=(b-a)/n, for some even integer n, and y(k) = f(a+kh). (Increasing n increases
;; the accuracy of the approximation). Define a procedure that takes as arguments f, a, b
;; and n and returns the value of the integral, computed using Simpson's Rule. Use your 
;; procedure to integrate "cube" between 0 and 1 (with n = 100 and n = 1000), and compare 
;; the results to those of the "integral" procedure shown above.
;;

;;
;; Definition of Simpson's Rule:
;;
(define (simpson f a b n)
  (define (even? k) (= (remainder k 2) 0))
  (define (odd? k) (not (even? k)))
  (define h (/ (- b a) n))
  (define (next x) (+ x h))
  (define (term a i n)
    (cond ((= i 0) (f a))
	  ((= i n) (f a))
	  ((even? i) (* 2 (f a)))
	  ((odd? i) (* 4 (f a)))))
  (define (sum term a next b i n)
    (if (> a b)
	0
	(+ (term a i n)
	   (sum term (next a) next b (+ i 1) n))))

  ;;
  ;; Check to make sure we have even n.
  ;;
  (if (even? n)
      (* (/ h 3) (sum term a next b 0 n))
      '()))
	   

;;
;; Now carry out the numerical integration on "cube" between 0 and 1 with n=100, n=1000
;; 
(define (cube x) (* x x x))

(simpson cube 0 1 100)
;; --> 1/4

(simpson cube 0 1 1000)
;; --> 1/4 

;;
;; This is the answer we were expecting.
;;

;;
;; We can perform the integration using real numbers:
;;
(simpson cube 0. 1. 100.)
;; --> 0.246666666666

(simpson cube 0. 1. 1000.)
;; --> 0.249666666666