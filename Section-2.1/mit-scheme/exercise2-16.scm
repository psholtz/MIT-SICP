;;
;; Exercise 2.16
;;
;; Explain, in general, why equivalent algebraic expressions may lead to different answers. 
;; Can you devise an interval-arithmetic package that does not have this shortcoming, or 
;; is this task impossible? (Warning: This problem is very difficult).
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

;;
;; Regarding the "parallel resistor" calculation specifically, consider the following
;; "law" of algebra, given c != 0:
;;
;;   1       c
;;  --- =  ----- 
;;   a      a*c
;;
;; The equivalence of the "par1" and "par2" resistor calculations depends upon this 
;; "law" holding true. But does it, when using interval arithmetic?
;;
;; Again, let's use "a" and "c" as defined above:
;;
(define a (make-interval 2 4))
(define c (make-interval 3 8))

;;
;; Carry out the calculation:
;;
(define one (make-interval 1 1))
(define x (div-interval one a))
;; ==> (0.25 . 0.5)
(define y (div-interval c (mul-interval a c)))
;; ==> (0.09375 . 1.3333333)

;;
;; Indeed, even the center points of the two intervals are off:
;;
(center x)
;; ==> 0.375
(center y)
;; ==> 0.7135416666

;;
;; If we cannot rely on this "law" as holding true in interval arithmetic, then the 
;; rest of the "algebra" used to assume the "equivalence" of the "par1" and "par2" 
;; procedures similarly collapses.
;;

;;
;; As a final point, it's worth going back to intuition I used in a previous
;; problem in this series. Namely, I was uncomfortable with using intervals 
;; that were defined as have "zero" width. To me, this didn't seem like an interval
;; in the proper sense of the word.
;;
;; Accordingly, I (re-)defined the constructor "make-interval" to check and make 
;; sure that the "higher" endpoint was greater than the "lower" endpoint:
;;
(define (make-interval a b)
  (cond ((< a b) (cons a b))
	(else
	 (display "error constructing interval!"))))

;;
;; If we use this as a constructor for building intervals, we cannot define an 
;; interval "one" as it is used in "par2", since the "one" interval defined in 
;; that procedure has zero width.
;;
;; Let's modify "par2" so that it uses a small epsilon value, and builds a small
;; "width" around the center point of "one":
;;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(define (par2-mod r1 r2)
  (define epsilon 0.1)
  (let ((one (make-interval (- 1 epsilon) (+ 1 epsilon))))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;
;; And let's return to one of the examples that gave rather divergent results
;; between "par1" and "par2":
;;
(define x (make-center-percent 1 0.1))
(define y (make-center-percent 2 0.1))

;;
;; The results are as follows:
;;
(par1 x y)
;; ==> (0.49090909 . 0.89629629)
(par2 x y)
;; ==> (0.6 . 0.7333333)
(par2-mod x y)
;; ==> (0.49090909 . 0.896296296296)

;;
;; So interestingly enough, adding just a small "epsilon" to the calculation 
;; for par2, modifies the procedure so that it returns a result more or less
;; identical to the result generated by par1.
;;