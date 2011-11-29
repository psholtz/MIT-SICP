;;
;; Exercise 2.23
;;

(define (for-each func items)
  (define (for-each-iter work answer)
    (if (null? work)
	'()
	(for-each-iter (cdr work)
		       (append answer
			       (list (func (car work)))))))
  (for-each-iter items '()))

