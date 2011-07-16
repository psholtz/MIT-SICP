;;
;; Exercise 2.17
;;

(define last-pair
  (lambda (x)
    (cond ((null? x) '())
	  ((null? (cdr x)) (list (car x)))
	  (else
	   (last-pair (cdr x))))))

      