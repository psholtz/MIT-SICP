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
;; Exercise 4
;;
;; Write a procedure, "build-balanced-tree", that takes a list of sorted elements, and 
;; returns a balanced binary tree of those elements, i.e., one in which "tree-lookup"
;; will run in O(lg n) time. Your solution (constructing the tree) may be slower than 
;; O(n) time, so long as lookups are fast.
;;
;; You may use the provided functions if you wish:
;;
;;  ;return the last k elements of lst
;;  (define (list-all lst k)
;;   (if (zero? k)
;;       lst
;;       (list-tail (cdr lst) (- k 1))))
;;
;;  ;return a list of the first k elements of 1
;;  (define (list-head lst k)
;;   (if (zero? k)
;;       '()
;;       (cons (car lst) (list-head (cdr lst) (- k 1)))))
;;
;;   ;lst must be sorted in increasing order
;;   (define (build-balanced-tree lst) ...
;;

;;
;; First import the provided procedures:
;;
(define (list-tail elems k)
  (if (zero? k)
      elems
      (list-tail (cdr elems) (- k 1))))

(define (list-head elems k)
  (if (zero? k)
      '()
      (cons (car elems) (list-head (cdr elems) (- k 1)))))

;;
;; Before we build the general tree itself, we need to know how to handle the special
;; cases of n=1 and n=2 trees. For n=1, the case is easy, we build a tree like:
;;
;;     (1)
;;     / \ 
;;    /   \
;;   ()   ()
;;
;; For n=2, we have a choice we of whether want to implement the tree as:
;;
;;      (2)
;;      / \
;;     /   \
;;   (1)    () 
;;   / \ 
;;  /   \
;; ()   ()
;;
;; Or whether we want to order it the other way around:
;;
;;      (1)
;;      / \ 
;;     /   \
;;    ()   (2)
;;         / \
;;        /   \
;;       ()   ()
;;
;; We'll make the choice of using the first model above.
;;
(define (build-balanced-tree elems)
  (let ((n (length elems)))
    (cond ((= n 1)
	   (make-tree-node (car elems)
			   the-empty-tree
			   the-empty-tree))
	  ((= n 2)
	   (make-tree-node (cadr elems)
			   (build-balanced-tree (list (car elems)))
			   the-empty-tree))
	  (else
	   (let ((k (quotient n 2)))
	     (make-tree-node (list-ref elems k)
			     (build-balanced-tree (list-head elems k))
			     (build-balanced-tree (list-tail elems (+ k 1)))))))))

;;
;; Run some unit tests:
;;
(build-balanced-tree '(1))
;; ==> (1 () ())
(build-balanced-tree '(1 2))
;; ==> (2 (1 () ()) ())
(build-balanced-tree '(1 2 3))
;; ==> (2 (1 () ()) (3 () ()))
(build-balanced-tree '(1 2 3 4 5))
;; ==> (3 (2 (1 () ()) ()) (5 (4 () ()) ()))
(build-balanced-tree '(1 2 3 4 5 6))
;; ==> (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) ()))
(build-balanced-tree '(1 2 3 4 5 6 7))
;; ==> (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))

;;
;; Test with spaces in the numbers:
;;
(build-balanced-tree '(1 3 5))
;; ==> (3 (1 () ()) (5 () ()))