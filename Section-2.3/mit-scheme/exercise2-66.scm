;;
;; Exercise 2.66
;;
;; Implement the "lookup" procedure for the case where the set of records is structured as a binary tree, 
;; ordered by the numerical values of the keys.
;;

;;
;; Again, we first need to import the supporting tree methods:
;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;
;; Convert a tree to a list:
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
;; Convert a list to a tree:
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
;; In order to unit test, we need a "key" procedure: [WORKING]
;;
(define (key record)
  (cond ((eq? record 1001) 1)
	((eq? record 2001) 2)
	((eq? record 3001) 3)
	((eq? record 4001) 4)
	((eq? record 5001) 5)
	(else
	 (error "Record not found! KEY"))))

;;
;; Let's define a tree of records. The "valid" records are 1001, 2001, 3001, 4001 and 5001. 
;; In our current set of records, let's just use 1001, 3001, 5001:
;;
(define records (list->tree '(1001 3001 5001)))

;;
;; We can define "lookup" as a recursive procedure:
;;
(define (lookup target records)
  (if (null? records)
      false
      (let ((current-record (entry records)))
	(let ((current-key (key current-record)))
	  (cond ((= target current-key) current-record)
		((< target current-key) (lookup target (left-branch records)))
		((> target current-key) (lookup target (right-branch records))))))))

;;
;; Let's see if we can "lookup" the records by key:
;;
(lookup 1 records)
;; ==> 1001
(lookup 2 records)
;; ==> #f
(lookup 3 records)
;; ==> 3001
(lookup 4 records)
;; ==> #f
(lookup 5 records)
;; ==> 5001     