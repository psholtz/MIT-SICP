;;
;; Exericse 2.15
;;
;; Eva Lu Ator, another user, has also noticed the different intervals computed by different but
;; algebraically equivalent expressions. She says that a formula to compute with intervals using
;; Alyssa's system will produce tighter error bounds if it can be written in such a form that no
;; variable that represents an uncertain number is repeated. Thus, she says, "par2" is a better
;; program for parallel resistances than "par1". Is she right? Why?
;;

;;
;; We observed in the last exercise that the "par2" procedure seemed to produce tighter
;; error bounds than the "par1" procedure. Let's go back and look more closely at this:
;;

;;
;; First look at the example with the extremely wide intervals:
;;
(define x (make-center-percent 10 0.3))
(define y (make-center-percent 10 0.4))

(/ (percent (par1 x y)) (percent (par2 x y)))
;; ==> 2.2727

;;
;; And let's look next at the example with the tighter intervals:
;;
(define x (make-center-percent 40000 0.005))
(Define y (make-center-percent 65000 0.0125))

(/ (percent (par1 x y)) (percent (par2 x y)))
;; ==> 3.453721

;;
;; So in both cases, the bounds produced by "par1" are about 2-3 times as wide as those
;; produced by "par2".
;;

;;
;; Let's take a closer look at the two procedures:
;;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;
;; One difference between the two procedures is that "par2" employs an interval in its computations, 
;; "one", which has no uncertainty. On the other hand, all the combinations created by "par1" involve
;; intervals with some finite measure of uncertainity. 
;;
;; Combining two intervals can never DECREASE the uncertainty in the resulting measurement, although 
;; it could possibly INCREASE that uncertainty. 
;;
;; For instance, suppose we have two intervals, both of width "a", and we add them together:
;;
;; (x-a,x+a) + (y-a,y+a)
;; ==>  (x+y-2a,x+y+2a)
;;
;; The resulting interval has a width of "2a".
;;
;; On the other hand, suppose we combine one interval of width "a" with a second interval having
;; zero uncertainty:
;;
;; (x,x) + (y-a,y+a)
;; ==> (x+y-a,x+y+a)
;;
;; The resulting interval has a width of only "a" this time.
;;
;; In this sense, using intervals that combine objects like "one" -- that have no uncertainty -- 
;; will produce a result that has "tighter" error bounds. 
;;

;;
;; Eva Lu Ator further suggested writing the code in such a way that no variable that represents
;; an uncertain number is repeated. One issue with repeating such uncertain numbers is that once a 
;; number has been specified, its value is no longer "uncertain" (so to speak). That is, "r1" has a 
;; concrete and "certain" value, it's just that we are not able to narrow this number down any further 
;; than by saying that it must occur in the range specified by the interval. 
;;
;; But the way "par1" is written, it treats the intervals "r1" and "r2" as being "equally" uncertain 
;; in both the "add-interval" and "mul-interval" procedures. Or said differently, the "r1" used in 
;; "add-interval" and "mul-interval" refers to the SAME resistor with the SAME DISTINCT uncertainty, 
;; not to TWO resistors whose values are known only to the same, EQUAL uncertainty. The way "par1" is 
;; written it models this latter case, although that will produce an overly pessimistic estimation 
;; of the uncertainty of the resulting interval.
;;
;; It's not necessarily clear or obvious how to write "par1" so that it models the first case, which 
;; would produce tighter error bounds, or if this  is even possible.
;;

;;
;; For the sake of interest, we can walk through a more quantitative analysis as well.
;;
;; Recall the definition of "div-interval":
;;
(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;; -------- 
;;   PAR1 
;; -------- 
;;
;; First let's step through the calculation for "par1".
;;
;; We are interested here in taking two intervals, r1 and r2, both modeled as having the same percentage
;; error "p", and understanding what the expected uncertainty in the resulting value given by "par1" 
;; should be.
;;
;; Suppose we have two intervals with the same percentage error:
;;
;; (x-a,x+a) and (y-b,y+b)
;;
;; where a = xp and b = yp, where p is the same in both equations.
;;
;; We could then write:
;;
;; [x(1-p),x(1+p)] and [y(1-p),y(1+p)] 
;;
;; for the two intervals, and adding them obtain:
;;
;; [(x+y)(1-p),(x+y)(1+p)]
;;
;; Suppose now that we multiply the two intervals together. This calculation was already carried out 
;; Exericse 2.13 but we repeat it (in essence) here:
;;
;; [x-a,x+a] * [y-b,y+b]
;; [xy - xb - ay + ab, xy + xb + ay + ab]
;; [xy - xy(b/y + a/x - ab/xy), xy + xy(b/y + a/x + ab/xy)]
;;
;; Ignoring the second order terms, and writing p = b/y = a/x, we have:
;;
;; [xy - xy(2p), xy + xy(2p)]
;; (xy(1-2p),xy(1+2p))
;;
;; In other words, if we take two intervals, each with percentage uncertainty "p", and multiply 
;; them together, the resulting interval will have percentage uncertainty "2p". 
;;
;; Finally, to calculate "par1", we must take an interval with percentage uncertainty "p" and 
;; divide it into an interval of percentage uncertainty "2p":
;;
;; (div-interval x y)
;; (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
;;
;; Consider first the second argument to "mul-interval".. the term "y" in this expression 
;; is the interval:
;;
;; [(x+y)(1-p),(x+y)(1+p)]
;;
;; Allowing z = x+y, we can write:
;;
;; [z(1-p),z(1+p)]
;; 
;; hence
;;
;; (make-interval (/ 1.0 (upper-bound y)) (/ 1.0  (lower-bound y)))
;; 
;; becomes
;;
;;   1.0      1.0
;;  ------ , ------ 
;;  z(1+p)   z(1-p)
;;
;; Recalling the Taylor expansion for f(z) = 1/z, we have:
;;
;; f(z+a) = f(z) + f'(z)*a + f''(z)*a^2/2 + ...
;;
;; f(z+a) = 1/z - 1/z^2*a + 2/z^3*a^2/2 - ...
;;
;; f(z+a) = 1/z( 1 - (a/z) + (a/z)^2 - ...)
;;
;; and since a = zp, we can write:
;;
;; f(z+a) = (1/z) * ( 1 - p + p^2 - ...)
;;
;; Keeping only the first-order terms, our expression for the argument interval becomes:
;;
;; [(1/z)*(1-p),(1/z)*(1+p)]
;;
;; Where z = x+y. 
;;
;; Returning now to our definition of "par1", it reduces to:
;;
;; (div-interval x y)
;;
;; Where the uncertainty in x is known to be (roughly) 2p, and the uncertainty in y is known to 
;; be (roughly) p. So the question is, if we multiply two intervals, one w/ uncertainty "2p" and 
;; the other with uncertainty "p", what is the uncertainty of the resulting interval? 
;;
;; The answer is straightforward:
;;
;; (1-2p,1+2p) x (1-p,1+p) 
;;
;; (1-3p+p^2,1+3p+p^2)
;;
;; Ignoring the second-order p^2 terms, the answer reduces to:
;;
;; (1-3p,1+3p)
;;
;; Or in other words, the resulting answer will have an uncertainty of "3p".
;;
;; We would thus expect, that it we supply "par1" with two intervals, each with the same percentage
;; uncertainty "p", that the resulting answer produced by "par1" will have an uncertainty of (roughly) "3p".
;;

;; -------- 
;;   PAR2  
;; -------- 
;;
;; Now let's step through the calculation for "par2".
;;
;; As before, we are interested in taking two intervals, r1 and r2, both modeled as having the 
;; same percentage uncertainty "p", and understanding what the expected uncertainty in the resulting
;; value given by "par2" should be. 
;;
;; First let's work through the expansion for (div-interval one r1), supposing that 
;; r1 is defined as the interval (x-a,x+a), and that p, the percentage uncertainty,
;; is given by a = xp. 
;;
;; As before, we have:
;; 
;; (div-interval one r1)
;; (mul-interval one (make-interval (/ 1.0 (upper-bound r1)) (/ 1.0 (lower-bound r1))))
;;
;; The interval which represents the second argument to "mul-interval" can be represented as:
;;
;;  1.0     1.0
;; ----- , ----- 
;;  x+a     x-a
;;
;; A Taylor expansion of f(x) = 1/x gives:
;;
;; f(x+a) = f(x) + f'(x)*a + f''(x)*a^2 / 2 + ...
;;
;; f(x+a) = 1/x - 1/x^2 * a + 2/x^3 *a^2 / 2 + ... 
;; 
;; f(x+a) = 1/x * ( 1 - a/x + (a/x)^2 - ...) 
;;
;; and since a = px, we can write:
;;
;; f(x+a) = 1/x * ( 1 - p + p^2 - ...)
;;
;; So to a first order approximation, we can write the interval as:
;;
;; [(1/x)(1-p),(1/x)(1+p)]
;;
;; In other words, the resulting interval has a percentage uncertainty of "p" (just as the initial
;; argument interval, r1, had).
;;
;; Similarly, the computation (div-interval one r2) will give (supposing that r2 is defined 
;; as the interval (y-b,y+b) where b=yp, and p is the same percentage uncertainty as in r1):
;;
;; [(1/y)(1-p),(1/y)(1+p)]
;;
;; Next we must combine these two intervals by adding. If we add two intervals, each with percentage
;; uncertainty of "p", the resulting interval will still have percentage uncertainty "p":
;;
;; [(x+y)/(xy)*(1-p),(x+y)/(xy)*(1+p)]
;;
;; Finally, we must divide this interval into "one" (the interval with no uncertainty), but as we have
;; already shown, dividing an interval with uncertainty "p" into "one" will produce a resulting interval
;; of uncertainty "p". Hence, we expect the final answer produced by "par2" to have an uncertainty of "p".
;;

;;
;; We can test this by considering two resistors, each with the same percentage error in uncertainty,
;; and seeing whether the results generated by "par1" and "par2" conform to the expectations we derived
;; above:
;;
(define x (make-center-percent 10000 0.1))
(define y (make-center-percent 12500 0.1))

;;
;; Note that we interval above has uncertainty 0.1 (10%).
;;
(percent (par1 x y))
;; ==> 0.292233
(percent (par2 x y))
;; ==> 0.1

;;
;; Which is what we expect.
;;

;;
;; So clearly, "par2" produces tighter error bounds than "par1". Whether this makes it a "better" 
;; program for parallel resistances depends on the nature of the project requirements.
;;