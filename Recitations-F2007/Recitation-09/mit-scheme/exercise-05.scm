;;
;; Working definitions
;;
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree-node-color value color left-subtree right-subtree)
  (list value color left-subtree right-subtree))
(define (make-tree-node-red value left-subtree right-subtree)
  (make-tree-node-color value 'red left-subtree right-subtree))
(define (make-tree-node-black value left-subtree right-subtree)
  (make-tree-node-color value 'black left-subtree right-subtree))

(define (node-value node)
  (car node))
(define (node-color node)
  (cadr node))
(define (node-left node)
  (caddr node))
(define (node-right node)
  (cadddr node))

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
;; Exercise 5
;; 
;; [WORKING]
;;

;;
;; One simple model for implementing self-balancing trees is a red-black tree.
;;

;;
;; Another relatively simple model for implementing self-balancing trees is 
;; an AVL tree, which we won't implement here. AVL trees are more strictly 
;; balanced than red-black trees, which means that insertion and retrieval is 
;; generally slower, but actual lookup of information is faster.
;;




;;
;; These trees have the properties that they "re-balance" their own nodes 
;; dynamically as nodes are added to or removed from the tree. More information
;; on AVL tres can be found on Google and Wikipedia.
;;
;; We will redefine "tree-insert" to function as an AVL tree.
;;
;; We give an implementation of AVL trees as follows:
;;

;;
;; AVL trees work by keeping the tree "as balanced" as possible at each node.
;; We define a "balance factor" at each node, which is (-1) x (weight of left branch) + 
;; (+1) x (weight of right branch). A balance factor of -1, 0, or 1 is considered
;; "balanced". Otherwise, we need to readjust the height of the tree.
;;
;; The first thing we need is a way to compute the height of the tree at a given node:
;;
(define (compute-height tree)
  (let ((h 0))
    (if (not (empty-tree (node-left tree)))
	(if (> 
  

;;
;; This information is derived (loosely) from the implementation found here:
;;
;; http://planet.racket-lang.org/package-source/oesterholt/datastructs.plt/1/0/html/datastructs-avl.html#top
;;