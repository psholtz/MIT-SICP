;;
;; Exercise 2.23
;;

(define (for-each func items)
  (define (iter list1)
    (cond ((null? list1) #t)
	  (else
	   (g (car list1))
	   (iter (cdr list1)))))
  (iter items))
