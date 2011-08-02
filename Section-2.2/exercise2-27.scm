;;
;; Exercise 2.27
;;

;;
;; The "reverse" procedure of 2.18
;;
(define (reverse original)
  (define (reverse-iter orig copy)
    (let ((new-copy (cons (car orig) copy)))
      (if (null? (cdr orig))
	  new-copy
	  (reverse-iter (cdr orig) new-copy))))
  (if (null? original)
      '()
      (reverse-iter original '())))


(define (deep-reverse original)
  (define (deep-reverse-iter orig copy)
    (cond ((null? (cdr orig)) new-copy)
	  (else
	   (display (length (car orig)))
	   (newline)
	   (deep-reverse-iter (cdr orig) new-copy))))
  (if (null? original)
      '()
      (deep-reverse-iter original '())))