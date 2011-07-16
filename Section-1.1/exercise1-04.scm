;;
;; Exercise 1.4
;;

;; Procedure adds absolute value of b to a. 
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;
;; Examples of procedure in use.
;;

(a-plus-abs-b 1 1)
;; returns 2

(a-plus-abs-b 1 -1)
;; returns 2

