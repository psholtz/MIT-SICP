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

(load-file "boolean-evaluator.el")

(eval-boolean (make-and t t) env)
;; ==> t
(eval-boolean (make-and t nil) env)
;; ==> nil
(eval-boolean (make-and nil t) env)
;; ==> nil
(eval-boolean (make-and nil nil) env)
;; ==> nil

(eval-boolean (make-or t t) env)
;; ==> t
(eval-boolean (make-or t nil) env)
;; ==> t
(eval-boolean (make-or nil t) env)
;; ==> t
(eval-boolean (make-or nil nil) env)
;; ==> nil [WORKING --> make sure this works]

(eval-boolean (make-not t) env)
;; ==> nil
(eval-boolean (make-not nil) env)
;; ==> t