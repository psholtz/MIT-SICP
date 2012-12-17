;;
;; A tree is either an empty tree, or a tree-node (defined below)
;;
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree-node value left-subtree right-subtree)
  (list value left-subtree right-subtree))

;;
;; Selectors
;;
(define (node-value node)
  (car node))
(define (node-left node)
  (cadr node))
(define (node-right node)
  (caddr node))

;;
;; Exercise 1
;; 
;; Complete the definition for "tree-lookup" which returns true if the value is present in the tree.
;;
(define (tree-lookup value tree)
  (cond ((not (tree? tree)) #f)
	((empty-tree? tree) #f)
	(else
	 (let ((current-value (node-value tree))
	       (left (node-left tree))
	       (right (node-right tree)))
	   (cond ((= current-value value) #t)
		 ((and (> current-value value) (not (empty-tree? left)))
		  (tree-lookup value left))
		 ((and (< current-value value) (not (empty-tree? right)))
		  (tree-lookup value right))
		 (else #f))))))

;;
;; Let's define a sample binary tree:
;;
(define n1 (make-tree-node 1 '() '()))
(define n2 (make-tree-node 4 '() '()))
(define n3 (make-tree-node 6 '() '()))

(define n4 (make-tree-node 3 n1 n2))
(define n5 (make-tree-node 9 n3 '()))

(define tree (make-tree-node 5 n4 n5))

tree
;; ==> (5 (3 (1 () ()) (4 () ())) (9 (6 () ()) ()))

;;
;; Let's see if it finds all the values/nodes:
;;
(tree-lookup 5 tree)
;; ==> #t
(tree-lookup 3 tree)
;; ==> #t
(tree-lookup 1 tree)
;; ==> #t
(tree-lookup 4 tree)
;; ==> #t
(tree-lookup 9 tree)
;; ==> #t
(tree-lookup 6 tree)
;; ==> #t

;;
;; Let's see what happens if we search for values not in the tree:
;;
(tree-lookup 33 tree)
;; ==> #f
(tree-lookup 1001 tree)
;; ==> #f
(tree-lookup -101 tree)
;; ==> #f

;;
;; What what happens if we misuse the API:
;;
(tree-lookup 31 44)
;; ==> #f
(tree-lookup 31 '())
;; ==> #f