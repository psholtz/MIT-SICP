;;
;; Exercise 2.22
;;

;; 
;; a "good" iterative version would be given by something like:
;;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		      (list (square (car things)))))))
  (iter items '()))

