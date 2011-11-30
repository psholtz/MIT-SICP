;;
;; Exercise 2.27
;;

;;
;; The "reverse" procedure of 2.18
;;
(define (reverse original)
  (if (null? original)
      '()
      (append (reverse (cdr original)) (list (car original)))))

;;
;;
;;
(define (deep-reverse original)
  (cond ((null? original) '())
	((pair? (car original))
	 (append (deep-reverse (cdr original)) (list (deep-reverse (car original)))))
	(else
	 (append (deep-reverse (cdr original)) (list (car original))))))
