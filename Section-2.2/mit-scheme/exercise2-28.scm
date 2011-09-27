;;
;; Exercise 2.28
;;

;;
;; this is a solution, but there may be better ones:
;;
(define (fringe x)
  (define (fringe-iter y)
    (cond ((number? y) (list y))
	  ((pair? y)
	   (append (fringe-iter (car y))
		   (fringe-iter (cdr y))))
	  (else '())))
  (fringe-iter x))



