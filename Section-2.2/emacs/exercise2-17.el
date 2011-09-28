;;
;; Exercise 2.17
;;
;; Define a procedure "last-pair" that returns the list that contains only the last 
;; element of a given (nonempty) list:
;;
;; (last-pair (list 23 72 149 34))
;;

;;
;; Define the "last-pair" procedure:
;;
(setq last-pair (lambda (x)
		  (cond ((null x) '())
			((null (cdr x)) (list (car x)))
			(t
			 (funcall last-pair (cdr x))))))

;;
;; Run some unit tests:
;;
(funcall last-pair '())
;; ==> nil
(funcall last-pair (list 1))
;; ==> (1)
(funcall last-pair (list 1 2))
;; ==> (2)
(funcall last-pair (list 1 2 3))
;; ==> (3)
(funcall last-pair (list 23 73 149 34))
;; ==> (34)