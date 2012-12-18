;;
;; A tree is either an empty tree, or a tree-node (defined below)
;;
(setq the-empty-tree '())
(defun empty-tree? (x) (null x))
(defun tree? (x) (listp x))

(defun make-tree-node (value left-subtree right-subtree)
  (list value left-subtree right-subtree))

;;
;; Selectors
;;
(defun node-value (node)
  (car node))
(defun node-left (node)
  (car (cdr node)))
(defun node-right (node)
  (car (cdr (cdr node))))

;;
;; Exercise 1
;; 
;; Complete the definition for "tree-lookup" which returns true if the value is present in the tree.
;;
(defun tree-lookup (value tree)
  (cond ((not (tree? tree)) '())
	((empty-tree? tree) '())
	(t
	 (let ((current-value (node-value tree))
	       (left (node-left tree))
	       (right (node-right tree)))
	   (cond ((= current-value value) t)
		 ((and (> current-value value) (not (empty-tree? left)))
		  (tree-lookup value left))
		 ((and (< current-value value) (not (empty-tree? right)))
		  (tree-lookup value right))
		 (else '()))))))

;; WORKING --> use cases 