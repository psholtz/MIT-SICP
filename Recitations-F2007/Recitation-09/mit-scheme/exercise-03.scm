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
;; Consider the three that results from evaluating the following:
;; 
;; (tree-insert 1 (tree-insert 2  ... (tree-insert n the-empty-tree)))
;;
;; What is the running time of calling "tree-lookup" on such a tree?
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

;;
;; If we invoke the procedure on just the first node, we see that the "outermost" make-tree-node
;; is invoked just once, to wit:
;;
(tree-insert 4 the-empty-tree)
(make-tree-node 4 '() '())
'(4 () ())

;;
;; When calculating the additional steps required for the next node, we see that the "outermost"
;; make-tree-node is invoked three times, to wit:
;;
(tree-insert 3 '(4 () ()))
(make-tree-node 4 (tree-insert 3 '()) '())
(make-tree-node 4 (make-tree-node 3 '() '()) '())
(make-tree-node 4 '(3 () ()) '())
'(4 (3 () ()) ())

;;
;; In the next node, the "outermost" make-tree-node is invoked five times, then 7 times, and 
;; so on. We deduce that the total number of steps required for n steps is:
;;
;;  n = 1 ==> 1 + 1 + 1 ==> 3
;;  n = 2 ==>     3 + 1 ==> 3 + 4 ==> 7
;;  n = 3 ==>     5 + 1 ==> 6 + 7 ==> 13
;;  n = 4 ==>     7 + 1 ==> 8 + 13 ==> 21
;;
;; and so on. 
;;
;; Breaking this down, we see that the total number of steps is 1 + n + (Summation of odd numbers).
;;
;; The sums of the first n odd numbers are easily calculated:
;;
;;  1 ==> 1 = 1^2
;;  1 + 3 ==> 4 = 2^2
;;  1 + 3 + 5 ==> 9 = 3^2
;;
;; Hence, the sum of the first n odd numbers is n^2. 
;;
;; Thus, the total number of steps required here is n^2 + n + 1. 
;;
;; We infer that the order of growth here is O(n^2).
;;