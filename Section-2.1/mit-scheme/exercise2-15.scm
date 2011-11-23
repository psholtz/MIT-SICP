;;
;; Exericse 2.15
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