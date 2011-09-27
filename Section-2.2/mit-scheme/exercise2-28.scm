;;
;; Exercise 2.28
;;
;; Write a procedure "fringe" that takes as arguments a tree (represented as a list) and 
;; returns a list whose elements are all the leaves of the tree arranged in left-to-right
;; order. For example, 
;; 
;; (define x (list (list 1 2) (list 3 4)))
;;
;; (fringe x)
;; (1 2 3 4)
;;
;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)
;;

;;
;; Define the "fringe" procedure:
;;
(define (fringe x)
  (define (fringe-iter y)
    (cond ((number? y) (list y))
	  ((pair? y)
	   (append (fringe-iter (car y))
		   (fringe-iter (cdr y))))
	  (else '())))
  (fringe-iter x))

;;
;; Run some unit tests:
;;
(define x (list (list 1 2) (list 3 4)))

(fringe x)
;; ==> (1 2 3 4)

(fringe (list x x))
;; ==> (1 2 3 4 1 2 3 4)


