;;
;; Exercise 2.13
;;
;; Show that under the assumption of small percentage tolerances there is a simple
;; formula for the approximate percentage tolerance of the product of two intervals
;; in terms of the tolerances of the facts. You may simplify the problem by assuming
;; that all numbers are positive.
;;

;;
;; Suppose we have two intervals, spanning intervals which are completely positive.
;;
;; Let the first interval be centered at "x", with width "a".
;;
;; Let the second interval be centered at "y", with width "b".
;;
(setq i1 (make-center-width 'x 'a))
(setq i2 (make-center-width 'y 'b))

;;
;; The intervals look like the following:
;;
;; i1 ==> (x-a,x+a)
;; i2 ==> (y-b,y+b)
;;

;;
;; Since all numbers are positive, the product of the two intervals is given by:
;;
;; i1 * i2 = ( (x-a)*(y-b), (x+a)*(y+b) )
;;
;; i1 * i2 = ( xy - (xb+ya) + ab, xy + (xb+ya) + ab )
;;

;;
;; Let's try to factor the product "xy" out of the last three terms:
;; 
;; i1 * i2 = ( xy - xy(b/y+a/x-ab/xy), xy + xy(b/y+a/x+ab/xy) )
;;

;;
;; We can define the relative percentage errors as follows:
;;
;; p1 = a/x
;; p2 = b/y
;;
;; It is given that these percentages are small, i.e., much less than 1 and close to 0.
;;
;; The product can be rewritten as:
;;
;; i1 * i2 = ( xy - xy(p1 + p2 - p1*p2), xy + xy(p1 + p2 + p1*p2) )
;;
;; Since the percentages are small, we can neglect p1*p2, which is second order.
;;
;; We are left with (the approximate relation):
;;
;; i1 * i2 = ( xy - xy(p1+p2), xy + xy(p1+p2) )
;;
;; i1 * i2 = ( xy * ( 1 - p1 - p2 ), xy * ( 1 + p1 + p2 ) )
;;