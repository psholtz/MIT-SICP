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
(define last-pair
  (lambda (x)
    (cond ((null? x) '())  ;; <-- should never hit this case, but "stop" just in case
	  ((null? (cdr x)) (list (car x)))
	  (else
	   (last-pair (cdr x))))))

;;
;; Run a unit test:
;;
(last-pair (list 23 72 149 34))
;; ==> (34)