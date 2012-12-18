;;
;; Working definitions
;;
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree-node value left-subtree right-subtree)
  (list value left-subtree right-subtree))

(define (node-value node)
  (car node))
(define (node-left node)
  (cadr node))
(define (node-right node)
  (caddr node))

;;
;; Previous exercises
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
;; Exercise 2
;;
;; Fill in the definition for "tree-insert" which takes in a tree and a val and returns a new 
;; tree with the value added.
;;
(define (tree-insert value tree)
  (cond ((empty-tree? tree)
	 (make-tree-node value
			 the-empty-tree
			 the-empty-tree))
	(else
	 (let ((current (node-value tree)))

	   (cond ((= value current) tree)
		 ((< value current)
		  (make-tree-node current
				  (tree-insert value (node-left tree))
				  (node-right tree)))
		 ((> value current)
		  (make-tree-node current
				  (node-left tree)
				  (tree-insert value (node-right tree)))))))))

;;
;; Let's run some tests:
;;
(define root the-empty-tree)
(define root (tree-insert 5 root))
;; ==> (5 () ())
(define root (tree-insert 5 root))
;; ==> (5 () ())
;; ^^ good, it returns just the same tree back again

(define root (tree-insert 3 root))
;; ==> (5 (3 () ()) ())
(define root (tree-insert 1 root))
;; ==> (5 (3 (1 () ()) ()) ())
(define root (tree-insert 4 root))
;; ==> (5 (3 (1 () ()) (4 () ())) ())
(define root (tree-insert 9 root))
;; ==> (5 (3 (1 () ()) (4 () ())) (9 () ()))
(define root (tree-insert 6 root))
;; ==> (5 (3 (1 () ()) (4 () ())) (9 (6 () ()) ()))

;;
;; Test to see if the values are in there:
;;
(tree-lookup 5 root)
;; ==> #t
(tree-lookup 3 root)
;; ==> #t
(tree-lookup 1 root)
;; ==> #t
(tree-lookup 4 root)
;; ==> #t
(tree-lookup 9 root)
;; ==> #t
(tree-lookup 6 root)
;; ==> #t

;;
;; Test for bad values:
;;
(tree-lookup 10 root)
;; ==> #f
(tree-lookup 33 root)
;; ==> #f

;;
;; Further information on binary trees is available here:
;; 
;; http://cslibrary.stanford.edu/110/BinaryTrees.html
;;



