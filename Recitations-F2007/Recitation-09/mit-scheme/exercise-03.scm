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
;; Exercise 3
;;
;; [WORKING]
;;

;;
;; We can define a procedure to generate such "linear" trees:
;;
(define (make-tree-linear n)
  (define (make-tree-linear-iter k working)
    (if (= k 1)
	(tree-insert k working)
	(make-tree-linear-iter (- k 1) (tree-insert k working))))
  (make-tree-linear-iter n the-empty-tree))

(make-tree-linear 1)
;; ==> (1 () ())
(make-tree-linear 2)
;; ==> (2 (1 () ()) ())
(make-tree-linear 3)
;; ==> (3 (2 (1 () ()) ()) ())

;;
;; Graphically these trees look like:
;;
;;
;;           3
;;          / \
;;         /   \
;;        2    ()
;;       / \
;;      /   \
;;     1    ()
;;    / \ 
;;   /   \
;;  ()   ()  
;;

;;
;; Let's expand the call graph:
;;
(tree-insert 1 (tree-insert 2 (tree-insert 3 (tree-insert 4 the-empty-tree))))
(tree-insert 1 (tree-insert 2 (tree-insert 3 (make-tree-node 4 '() '()))))
(tree-insert 1 (tree-insert 2 (tree-insert 3 '(4 () ()))))
(tree-insert 1 (tree-insert 2 (make-tree-node 4 (tree-insert 3 '()) '())))
(tree-insert 1 (tree-insert 2 (make-tree-node 4 (make-tree-node 3 '() ()) '())))
(tree-insert 1 (tree-insert 2 (make-tree-node 4 '(3 () ())) '()))
(tree-insert 1 (tree-insert 2 '(4 (3 () ()) ())))
(tree-insert 1 (make-tree-node 4 (tree-insert 2 '(3 () ())) '()))
(tree-insert 1 (make-tree-node 4 (make-tree-node 3 (tree-insert 2 '() '()) '()) '()))
(tree-insert 1 (make-tree-node 4 (make-tree-node 3 (make-tree-node 2 '() '()) '()) '()))
(tree-insert 1 (make-tree-node 4 (make-tree-node 3 '(2 () ()) '()) '()))
(tree-insert 1 (make-tree-node 4 '(3 (2 () ()) ()) '()))
(tree-insert 1 '(4 (3 (2 () ()) ()) ()))
(make-tree-node 4 (tree-insert 1 '(3 (2 () ()) ())) '())
(make-tree-node 4 (make-tree-node 3 (tree-insert 1 '(2 () ())) '()) '())
(make-tree-node 4 (make-tree-node 3 (make-tree-node 2 (tree-insert 1 '() '()) '()) '()) '())
(make-tree-node 4 (make-tree-node 3 (make-tree-node 2 (make-tree-node 1 '() '()) '()) '()) '())
(make-tree-node 4 (make-tree-node 3 (make-tree-node 2 '(1 () ()) '()) '()) '())
(make-tree-node 4 (make-tree-node 3 '(2 (1 () ()) ()) '()) '())
(make-tree-node 4 '(3 (2 (1 () ()) ()) ()) '())
'(4 (3 (2 (1 () ()) ()) ()) ())
