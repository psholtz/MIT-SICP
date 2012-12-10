;;
;; Exercise 2.66
;;
;; [WORKING]
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
  'A)

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
;; (could also give iterative definition);
;;
	