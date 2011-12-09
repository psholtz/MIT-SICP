;;
;; Exercise 2.58
;;

(define (union-set s1 s2)
  (cond ((null? s1) s2)
	((element-of-set? (car s1) s2)
	 (union-set (cdr s1) s2))
	(else
	 (union-set (cdr s1) (cons (car s1) s2)))))
