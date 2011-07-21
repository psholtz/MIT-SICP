;;
;; Exercise 1.4
;;
;; Observe that our model of evaluation allows for combinations whose operators are compound expressions. 
;; Use this observation to describe the behavior of the following procedure:
;;
;; (define (a-plus-abs-b a b)
;;  ((if (> b 0) + -) a b))
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

