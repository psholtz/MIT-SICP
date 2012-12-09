;;
;; Exercise 2.63
;;
;; [WORKING]
;;

;;
;; First define some of the supporting procedures:
;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;;
;; Define first tree->list procedure:
;;
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

;;
;; Define second tree->list procedure:
;;
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;;
;; Let's build the data structures as given in the text:
;;
(define tree1 (adjoin-set 7 '()))
(define tree1 (adjoin-set 3 tree1))
(define tree1 (adjoin-set 1 tree1))
(define tree1 (adjoin-set 5 tree1))
(define tree1 (adjoin-set 9 tree1))
(define tree1 (adjoin-set 11 tree1))

(define tree2 (adjoin-set 3 '()))
(define tree2 (adjoin-set 1 tree2))
(define tree2 (adjoin-set 7 tree2))
(define tree2 (adjoin-set 5 tree2))
(define tree2 (adjoin-set 9 tree2))
(define tree2 (adjoin-set 11 tree2))

(define tree3 (adjoin-set 5 '()))
(define tree3 (adjoin-set 3 tree3))
(define tree3 (adjoin-set 1 tree3))
(define tree3 (adjoin-set 9 tree3))
(define tree3 (adjoin-set 7 tree3))
(define tree3 (adjoin-set 11 tree3))	

;;
;; Let's run the first procedure on the three trees:
;;
(tree->list-1 tree1)
;; ==> (1 3 5 7 9 11)
(tree->list-1 tree2)
;; ==> (1 3 5 7 9 11)
(tree->list-1 tree3)
;; ==> (1 3 5 7 9 11)

;;
;; Let's run the second procedure on the three trees:
;;
(tree->list-2 tree1)
;; ==> (1 3 5 7 9 11)
(tree->list-2 tree2)
;; ==> (1 3 5 7 9 11)
(tree->list-2 tree3)
;; ==> (1 3 5 7 9 11)

;;
;; The two procedures produce the same results for all three trees.
;;

;;
;; Let's expand the call graph for the first procedure on the first tree:
;;
(tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(append (tree->list-1 '(3 (1 () ()) (5 () ()))) (cons 7 (tree->list-1 '(9 () (11 () ())))))
(append (append (tree->list-1 '(1 () ())) (cons 3 (tree->list-1 '(5 () ())))) (cons 7 (append (tree->list-1 '()) (cons 9 (tree->list-1 '(11 () ()))))))
(append (append (append (tree->list-1 '()) (cons 1 (tree->list-1 '()))) (cons 3 (append (tree->list-1 '()) (cons 5 (tree->list-1 '())))))
	(cons 7 (append '() (cons 9 (append (tree->list-1 '()) (cons 11 (tree->list-1 '())))))))
(append (append (append '() (cons 1 '())) (cons 3 (append '() (cons 5 '()))))
	(cons 7 (append '() (cons 9 (append '() (cons 11 '()))))))
(append (append '(1) (cons 3 '(5))) (cons 7 (append '() (cons 9 '(11)))))
(append (append '(1) '(3 5)) (cons 7 '(9 11)))
(append '(1 3 5) '(7 9 11))
'(1 3 5 7 9 11)

;;
;; Let's expand the call graph for the second procedure on the first tree:
;;
(tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(copy-to-list '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))) '())
(copy-to-list '(3 (1 () ()) (5 () ())) (cons 7 (copy-to-list '(9 () (11 () ())) '())))
(copy-to-list '(1 () ()) (cons 3 (copy-to-list '(5 () ()) (cons 7 (copy-to-list '(9 () (11 () ())) '())))))
(copy-to-list '() (cons 1 (copy-to-list '() (cons 3 (copy-to-list '(5 () ()) (cons 7 (copy-to-list '(9 () (11 () ())) '())))))))
(cons 1 (copy-to-list '() (cons 3 (copy-to-list '(5 () ()) (cons 7 (copy-to-list '(9 () (11 () ())) '()))))))
(cons 1 (cons 3 (copy-to-list '(5 () ()) (cons 7 (copy-to-list '(9 () (11 () ())) '())))))
(cons 1 (cons 3 (copy-to-list '() (cons 5 (cons 7 (copy-to-list '(9 () (11 () ())) '()))))))
(cons 1 (cons 3 (cons 5 (cons 7 (copy-to-list '(9 () (11 () ())) '())))))
(cons 1 (cons 3 (cons 5 (cons 7 (copy-to-list '() (cons 9 (copy-to-list '(11 () ()) '())))))))
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (copy-to-list '(11 () ()) '()))))))
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (copy-to-list '() (cons 11 (copy-to-list '() '()))))))))
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11 (copy-to-list '() '())))))))
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11 '()))))))
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 '(11))))))
(cons 1 (cons 3 (cons 5 (cons 7 '(9 11)))))
(cons 1 (cons 3 (cons 5 '(7 9 11))))
(cons 1 (cons 3 '(5 7 9 11)))
(cons 1 '(3 5 7 9 11))
'(1 3 5 7 9 11)

;;
;; Will the two procedures always generate the same list?
;;
;; The first procedure works something like the following:
;; 
;;  (1) Generate a list based on the left branch;
;;  (2) Determine what the center, or "entry", element is;
;;  (3) Generate a list based on the right branch;
;;  (4) Cons the result of Step (2) to the list generated in Step (3);
;;  (5) Append the list generated in Step (1) to the list generated in Step (4);
;; 
;; Because the binary tree is structured based on the notion of ordering, 
;; which is to say, the elements in the left branch are always smaller than the 
;; center element, which is always smaller than the elements in the right branch, 
;; for this reason, this algorithm will take a tree and generate a list that 
;; is sorted in increasing order.
;;
;; The second procedure works something like the following:
;;
;; (1) If the tree is null, return the generated list that has been built up thus far;
;; (2) Determine what the left branch is;
;; (3) Determine what the center element is;
;; (4) Determine what the right branch is;
;; (5) Recursively invoke the procedure using the right branch (Step (4)) as the tree 
;;     and the list that has been generated thus far.
;; (6) Cons the result of Step (3) to the list that is generated in Step (5) 
;; (7) Recursively invoke the procedure using the left branch (Step (2)) as the tree 
;;     and the list generated in Step (6) as the list generated thus far.
;;
;; Like the first procedure, will procedure will generated a list whose elements are the 
;; nodes in the tree, and whose elements are sorted in increasing order.
;;
;; Thus, both procedures will always terminate with the same result: a list of the nodes
;; in the tree, sorted in increasing order.
;;

;;
;; To calculate the relative performance of the two procedures, let's look at the second one first:
;;
;; Suppose we have a balanced tree with n nodes, and we apply it to the "tree->list-2" procedure.
;;
;; At each step of the algorithm, an iterative call is made to "tree->list-2" twice, once for 
;; the left branch and once for the right branch, but in each case the call is made with a 
;; list that is only n/2 in size.
;;
;; For instance, the call graph for a tree with 2 nodes will break down something like the following:
;;
;;            (f 2) 
;;      (f 1)      (f 1)
;;  (f 0) (f 0) (f 0) (f 0)
;;
;; The call graph is 3 steps deep. Calling the procedure with a tree with 4 nodes will generate a call
;; graph that is 4 steps deep. Calling the procedure with a tree 8 with 8 nodes will generate a call
;; graph that is 5 steps deep, and so on. The total "height" of the call graph, in other words, is 
;; 2 + log_2(n), where n is the size of the (balanced) tree.
;; 
;; We surmise, then, that tree->list-2 will execute in roughly O(log(n)) time. 
;;
;; The performance of "tree->list-1" is similar to this, but for each iteration step, an additional call 
;; to "append" is made. Since "append" runs in linear, or O(n) time, we would expect tree->list-1 to perform
;; slower on larger trees than tree->list-2. Specifically, we would expect tree->list-1 to execute 
;; in roughly O(n*log(n)) time.
;;