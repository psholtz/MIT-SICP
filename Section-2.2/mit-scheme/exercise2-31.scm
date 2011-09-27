;;
;; Exercise 2.31
;;
;; Abstract your answer from Exercise 2.30 to produce a procedure "tree-map" with the property
;; that "square-tree" could be defined as:
;;
;; (define (square-tree tree) (tree-map square tree))
;;

;;
;; Define the "tree-map" procedure directly:
;;
(define (tree-map f tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (f tree))
	(else
	 (cons (tree-map f (car tree))
	       (tree-map f (cdr tree))))))

;;
;; Define "square-tree" in terms of "tree-map":
;;
(define (square-tree tree) (tree-map square tree))

;;
;; Run a unit test:
;;
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; ==> (1 (4 (9 16) 25) (36 49))

;;
;; We can also define "tree-map" recursively in terms of map and lambda:
;;
(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))

;;
;; Again, define "square-tree" in terms of "tree-map":
;;
(define (square-tree tree) (tree-map square tree))

;;
;; Run a unit test:
;;
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; ==> (1 (4 (9 16) 25) (36 49))