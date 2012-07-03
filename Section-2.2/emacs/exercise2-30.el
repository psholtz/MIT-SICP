;;


;;
;; Define the "square" form:
;;
(defun square (n) (* n n))

;;
;; Define "square-tree" directly:
;;
(defun square-tree (tree)
  (cond ((null tree) '())
	((not (listp tree)) (square tree))
	(t
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

;;
;; Run some unit tests:
;;
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; ==>

;;
;; Define "square-tree" using map and lambda:
;;
(defun square-tree (tree)
  (mapcar (lambda (sub-tree)
	    (if (listp sub-tree)
		(square-tree sub-tree)
	      (square sub-tree)))
	  tree))

;;
;; Run some unit tests:
;;
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; ==>