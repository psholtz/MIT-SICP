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
  ;;
  ;; Define an iterative algorithm for inserting values
  ;;
  (define (tree-insert-iter subtree)
    (let ((current-value (node-value tree))
	  (left (node-left tree))
	  (right (node-right tree)))
      (if (> current-value value)
	  (if (empty-tree? left)
	      (make-tree-node value '() '())
	      (tree-insert-iter left)))
      (if (< current-value value)
	  (if (empty-tree? right)
	      '()
	      (tree-insert-iter right)))))

  ;; 
  ;; Invoke the iterative algorithm (if necessary)
  ;;			       
  (if (empty-tree? tree)
      (make-tree-node value '() '())
      (tree-insert-iter tree)))


;;
;; WORKING
;;