;;
;; Exercise 2.20
;;

(define (same-parity x . z)
  ;; define the parity-testing procedures
  (define (even? n) (= (remainder n 2) 0))
  (define (odd? n) (not (even? n)))

  ;; define the list-filtering routine
  (define (filter func)
    (define (filter-iter list-in list-out)
      (cond ((null? list-in) list-out)
	    (else
	     (let ((a (car list-in)))
	       (if (func a)
		   (filter-iter (cdr list-in) (append list-out (list a)))
		   (filter-iter (cdr list-in) list-out))))))
    (filter-iter z (list x)))
  
  ;; invoke the "filter" procedure appropriately
  (cond ((even? x) (filter even?))
	(else (filter odd?))))

	     