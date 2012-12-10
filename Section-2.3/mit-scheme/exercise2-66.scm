;;
;; Exercise 2.66
;;
;; [WORKING]
;;

(define (lookup key records)
  (cond ((null? records) #f)
	((
	