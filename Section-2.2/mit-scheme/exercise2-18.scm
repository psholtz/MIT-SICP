


(define (reverse original)
  (if (null? original)
      '()
      (reverse-iter original '())))

(define (reverse-iter original copy)
  (let ((new-copy (cons (car original) copy)))
    (if (null? (cdr original))
	new-copy
	(reverse-iter (cdr original) new-copy))))

