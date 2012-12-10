;;
;; Exercise 2.65
;;
;; Use the results of Exercises 2.63 and 2.64 to give O(n) implementations of "union-set"
;; and "intersection-set" for sets implemented as (balanced) binary trees.
;;

;;
;; First let's import the supporting tree operations that we're going to require:
;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;
;; Let's also import our favorite "tree->list" procedure from Exercise 2.63:
;;
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;;
;; And let's also import the "list->tree" procedure from Exercise 2.64:
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
;; Now we need to include the O(n) set operations we've defined 
;; earlier, for both "union" and "intersection". These operate
;; on ordered sets (in linear time):
;;
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((elem1 (car set1))
	       (elem2 (car set2)))
	   (cond ((= elem1 elem2)
		  (cons elem1 (union-set (cdr set1) (cdr set2))))
		 ((< elem1 elem2)
		  (cons elem1 (union-set (cdr set1) set2)))
		 ((> elem1 elem2)
		  (cons elem2 (union-set set1 (cdr set2)))))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((> x1 x2)
	       (intersection-set set1 (cdr set2)))))))

;;
; Finally, let's define the "union-set" and "intersection-set" procedures:
;;

(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1)
			 (tree->list tree2))))

(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1)
				(tree->list tree2))))

;;
;; Let's run some unit tests:
;;
(define t1 (list->tree '(1)))
;; ==> (1 () ())
(define t2 (list->tree '(1 2)))
;; => (1 () (2 () ()))
(define t3 (list->tree '(1 2 3)))
;; ==> (2 (1 () ()) (3 () ()))
(define t4 (list->tree '(2 3 4)))
;; ==> (3 (2 () ()) (4 () ()))
(define t5 (list->tree '(4 5 6)))
;; ==> (5 (4 () ()) (6 () ()))

(union-tree '() '())
;; ==> ()
(union-tree t1 '())
;; ==> (1 () ())
(union-tree '() t1)
;; ==> (1 () ())
(union-tree (list->tree '(1)) (list->tree '(1)))
;; ==> (1 () ())
(union-tree (list->tree '(1 2)) (list->tree '(1 2)))
;; ==> (1 () (2 () ()))
(union-tree t1 t2)
;; ==> (1 () (2 () ()))
(union-tree t2 t1)
;; ==> (1 () (2 () ()))
(union-tree t3 t4)
;; ==> (2 (1 () ()) (3 () (4 () ())))
(union-tree t3 t5)
;; ==> (3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))

(intersection-tree '() '())
;; ==> ()
(intersection-tree t1 '())
;; ==> ()
(intersection-tree '() t1)
;; ==> ()
(intersection-tree (list->tree '(1)) (list->tree '(1)))
;; ==> (1 () ())
(intersection-tree (list->tree '(1 2)) (list->tree '(1 2)))
;; ==> (1 () (2 () ()))
(intersection-tree t1 t2)
;; ==> (1 () ())
(intersection-tree t2 t1)
;; ==> (1 () ())
(intersection-tree t3 t4)
;; ==> (2 () (3 () ()))
(intersection-tree t3 t5)
;; ==> ()

;; 
;; Why do these procedures run in O(n) time?
;;
;; Consider the "union-tree" procedure. The "tree->list" procedure runs in O(n) time, and this 
;; procedure is called twice. The "union-set" procedure runs in O(n), and the final call to 
;; "list->tree" also runs in O(n) time. So we have four procedure calls, each of which run in 
;; linear time. Invoking the "union-tree" may, therefore, take (up to) 4 times longer than 
;; executing any single one of these O(n) procedures, but the sum of four O(n) is still a procedure
;; which runs in linear O(n) time.
;;
;; The same arguments can be made for "intersection-tree".
;;