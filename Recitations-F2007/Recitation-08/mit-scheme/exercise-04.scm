;;
;; Exercise 4
;; 
;; The evaluator as described so far only allows expressions to be either boolean operators
;; or variable values. Extend the operator so that expressions can include literal boolean as 
;; well, so that evaluating expressions such as (and #t #f) work.
;;

;;
;; Our definition of the boolean evaluator already meets these requirements:
;;

(load "boolean-evaluator.scm")

(eval-boolean (make-and #t #t) env)
;; ==> #t
(eval-boolean (make-and #t #f) env)
;; ==> #f
(eval-boolean (make-and #f #t) env)
;; ==> #f
(eval-boolean (make-and #f #f) env)
;; ==> #f

(eval-boolean (make-or #t #t) env)
;; ==> #t
(eval-boolean (make-or #t #f) env)
;; ==> #t
(eval-boolean (make-or #f #t) env)
;; ==> #t
(eval-boolean (make-or #f #f) env)
;; ==> #f

(eval-boolean (make-not #t) env)
;; ==> #f
(eval-boolean (make-not #f) env)
;; ==> #t
