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