;;
;; Exercise 2.64
;; 
;; The following procedure "list->tree" converts an ordered list to a balanced binary tree. The helper
;; procedure "partial-tree" takes as arguments an integer n and list of at least n elements and constructs 
;; a balanced tree containing the first n elements of the list. The result returned by "partial-tree" is 
;; a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not 
;; included in the tree.
;;
;;  (define (list->tree elements)
;;   (car (partial-tree elements (length elements))))
;;
;;  (define (partial-tree elts n)
;;   (if (= n 0)
;;    (cons '() elts)
;;    (let ((left-size (quotient (- n 1) 2)))
;;     (let ((left-result (partial-tree elts left-size)))
;;      (let ((left-tree (car left-result))
;;            (non-left-elts (cdr left-result))
;;            (right-size (- n (+ left-size 1))))
;;       (let ((this-entry (car non-left-elts))
;;             (right-result (partial-tree (cdr non-left-elts)
;;                                          right-size)))
;;        (let ((right-tree (car right-result))
;;              (remaining-elts (cdr right-result)))
;;         (cons (make-tree this-entry left-tree right-tree)
;;               remaining-elts))))))))
;;
;; (a) Write a short paragraph explaining as clearly as you can how "partial-tree" works. Draw the tree produced 
;;     by "list->tree" for the list (1 3 5 7 9 11).
;;
;; (b) What is the order of growth in the number of steps required by "list->tree" to convert a list of "n" elements?
;;

;;
;; First import the supporting procedures:
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
;; Now define the current procedures:
;;
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;;
;; Let's experiment to see how the procedure generates trees from lists:
;;
(list->tree '(1 2 3))
;; ==> (2 (1 () ()) (3 () ()))

(list->tree '(1 3 5 7 9 11))
;; ==> (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;;
;; We can graphically represent this tree as:
;;
;;        5
;;       / \
;;      /   \
;;     /     \ 
;;    1       9
;;     \     / \ 
;;      3   7   11
;;
;; Note that this is different from any of the three trees given in the text
;; for these ordered list.
;;

;;
;; To get a sense for what "partial-tree" actually does, let's invoke the 
;; procedure (partial-tree '(1 2 3 4) n), with n varying from 0 to 4:
;;
(partial-tree '(1 2 3 4) 0)
;; ==> (() 1 2 3 4)
(partial-tree '(1 2 3 4) 1)
;; ==> ((1 () ()) 2 3 4)
(partial-tree '(1 2 3 4) 2)
;; ==> ((1 () (2 () ())) 3 4)
(partial-tree '(1 2 3 4) 3)
;; ==> ((2 (1 () ()) (3 () ())) 4)
(partial-tree '(1 2 3 4) 4)
;; ==> ((2 (1 () ()) (3 () (4 () ()))))

;;
;; This is quite an amazing procedure: suppose, for the sake of discussion, 
;; the signature of the procedure is (partial-tree elements n). Then invoking 
;; this procedure will return a list where the first element is a binary tree
;; built from the first n members of "elements", and the remaining elements 
;; of the list are the remaining (- (length element) n) members of elements that 
;; have not yet been incorporated into the tree!
;;
;; So invoking (partial-tree '(1 2 3 4) 1) returns a list where the first element
;; is a tree consisting only of the number 1, and the remaining 3 elements in the 
;; list are 2 3 4, that is, the members of elements that were not yet incorporated
;; into the tree.
;;
;; Invoking (partial-tree '(1 2 3 4) 4) will return a list with just one element, 
;; the fully formed binary tree consisting of all four members of elements.
;;
;; Let's explore a few more things about (partial-tree elements n). Specifically, 
;; let's invoke the procedure on lists that have (- n 1) elements, where n is a 
;; a power of two:
;;
(car (partial-tree '(1 2 3) 3))
;; ==> (2 (1 () ()) (3 () ()))

(car (partial-tree '(1 2 3 4 5 6 7) 7))
;; ==> (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))

(partial-tree '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) 15)
;; ==> (8 (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ()))) (12 (10 (9 () ()) (11 () ())) (14 (13 () ()) (15 () ()))))

;;
;; Let's draw the first two trees:
;;
;;    2
;;   / \
;;  1   3
;;
;; and
;;
;; 
;;        4 
;;       / \
;;      /   \
;;     /     \
;;    2       6 
;;   / \     / \
;;  1   3   5   7
;;
;; The reader can convince himself that the binary tree generated by the third procedure call above
;; also generates a fully balanced binary tree. 
;; 
;; In other words, if given a "full" set of data to work with, "partial-tree" will generate
;; a fully balanced binary tree!
;;

;;
;; Let's analyze the performance of this algorithm:
;;
;; The nesting of the "let" statements in the procedure makes reading it a bit intimidating, but
;; in essence the function performed by the procedure is extremely simple: again, suppose we 
;; invoke the procedure as (partial-tree elements n):
;;
;;  (1) The procedure "cuts" the elements list (roughly) in half, and generates a balanced binary tree 
;;      from the "left" half, labeling this "left-tree".
;;  (2) The "center" element of the list is labeled "this-entry".
;;  (3) The procedure takes the "right" elements of the list, and generated a balanced binary tree
;;      using them, labeling this "right tree".
;; 
;; These three elements are "glued together" in order using "make-tree", which runs in constant time.
;;
;; Any additional "gluing" of lists is accomplished using "cons", which also runs in constant time. 
;;
;; The order of growth for the "partial-tree" procedure is linear in the size of the argument 
;; list. Specifically, if the size of the argument list is "n", then "partial-tree" will be invoked
;; exactly 2*n + 1 times: twice for each element in the list, since the procedure searches for both 
;; a left and right branch emanating from each node, and once for the original invocation to initiate
;; the recursion. 
;;
;; A small python script has been written which counts the invocations, and is included.
;;
;; Additional operation incurred by this procedure are in constant time.
;;
;; Hence, the order of growth for the procedure is linear, or O(n).
;;