;;
;; Exercise 2.16
;;

;;
;; The first thing I did was look around to see if there are other "interval arithmetic" packages
;; in commercial use that may exhibit the same "error" we've just encountered. 
;;
;; Interestingly, the Boost C++ libraries have their own template library "interval" which implements
;; interval arithmetic. 
;;
;; I ran some examples of interval arithmetic through the Boost C++ libraries, and posted the results
;; in the Boost C++ branch of this repository. Indeed, the Boost libraries give the same answers and
;; results that we obtained with the Scheme libraries derived in the SICP text.
;;
;; Specifically, the Boost C++ libraries exhibit the same "error" when computing parallel resistances 
;; that we encountered with the Scheme code.
;;

;;
;; Why do "equivalent" algebraic expressions lead to "different" answers? 
;;
;; One answer is that the algebra we must use when performing interval arithmetic is not the same "algebra"
;; with which most people are familiar from grade school. For example, interval arithmetic is not 
;; distributive, but rather sub-distributive -- that is a(b+c) does NOT necessarily equal ab+ac. 
;; Rather, the best we can say is that a(b+c) will be a subset of ab+ac. 
;;
;; So if we expect a(b+c) to be "algebraically equivalent" to ab+ac, we are bound to be disappointed
;; when working with interval arithmetic.
;;

;;
;; Let's define the a, b and c for distribution:
;;
(define a (make-interval 2 4))
(define b (make-interval -2 0))
(define c (make-interval 3 8))

;;
;; Let's look at the two expressions for distribution, which naively, we might expect to be identical:
(define x (mul-interval a (add-interval b c)))
;; ==> (2 . 32)
(define y (add-interval (mul-interval a b) (mul-interval a c)))
;; ==> (-2 . 32) 

(lower-bound x)
;; ==> 2
(upper-bound x)
;; ==> 32

(lower-bound y)
;; ==> -2
(upper-bound y)
;; ==> 32

;; 
;; So indeed, x is a subset of y, and hence the algebra is sub-distributive, but clearly
;; the two intervals are not "identical".
;;
;; Source: http://en.wikipedia.org/wiki/Interval_arithmetic
;;

;;
;; As another example, consider the interval [-2,2]. 
;;
;; In "normal" algebra, we expect negative numbers to square to a positive number.
;;
;; Squaring [-2,2] gives us a different result:
;;
(define a (make-interval -2 2))
(mul-interval a a)
;; ==> (-4 . 4)

;;
;; So the "normal" laws of algebra do not apply when working with interval arithmetic.
;;