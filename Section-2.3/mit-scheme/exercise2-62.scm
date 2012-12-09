;;
;; Exercise 2.62
;;
;; Give an O(n) implementation of "union-set" for sets represented as ordered lists.
;;

;;
;; We can follow a similar pattern as was given in the text for "intersection-set":
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
		 ((< elem2 elem1)
		  (cons elem2 (union-set set1 (cdr set2)))))))))

;;
;; Unit tests:
;;
(union-set '() '())
;; ==> ()
(union-set '(1) '())
;; ==> (1)
(union-set '() '(1))
;; ==> (1)
(union-set '(1) '(1))
;; ==> (1)

(union-set '(1 2 3) '())
;; ==> (1 2 3)
(union-set '() '(1 2 3))
;; ==> (1 2 3)
(union-set '(1 2 3) '(1 2 3))
;; ==> (1 2 3)

(union-set '(1 2 3) '(4 5 6))
;; ==> (1 2 3 4 5 6)
(union-set '(4 5 6) '(1 2 3))
;; ==> (1 2 3 4 5 6)

(union-set '(1 3 5) '(2 4 6))
;; ==> (1 2 3 4 5 6)
(union-set '(2 4 6) '(1 3 5))
;; ==> (1 2 3 4 5 6)

(union-set '(1 3 10) '(4 5 9))
;; ==> (1 3 4 5 9 10)
(union-set '(4 5 9) '(1 3 10))
;; ==> (1 3 4 5 9 10)

(union-set '(1 2 3 4) '(3 4 5 6))
;; ==> (1 2 3 4 5 6)
(union-set '(3 4 5 6) '(1 2 3 4))
;; ==> (1 2 3 4 5 6)


(union-set '(1 2 3 4 8) '(3 4 5 6 8))
;; ==> (1 2 3 4 5 6 8)
(union-set '(3 4 5 6 8) '(1 2 3 4 8))
;; ==> (1 2 3 4 5 6 8)

;;
;; Let's walk through a call graph, to get a sense of the order of performance statistics:
;;
(union-set '(1 2 3 4 8) '(3 4 5 6 8))
(cons 1 (union-set '(2 3 4 8) '(3 4 5 6 8)))
(cons 1 (cons 2 (union-set '(3 4 8) '(3 4 5 6 8))))
(cons 1 (cons 2 (cons 3 (union-set '(4 8) '(4 5 6 8)))))
(cons 1 (cons 2 (cons 3 (cons 4 (union-set '(8) '(5 6 8))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (union-set '(8) '(6 8)))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (union-set '(8) '(8))))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 8 '())))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 '(8)))))))
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(6 8))))))
(cons 1 (cons 2 (cons 3 (cons 4 '(5 6 8)))))
(cons 1 (cons 2 (cons 3 '(4 5 6 8))))
(cons 1 (cons 2 '(3 4 5 6 8)))
(cons 1 '(2 3 4 5 6 8))
'(1 2 3 4 5 6 8)

;;
;; The number of steps will be roughly proportional to 2*(n1+n2), where n1 and n2 are the 
;; sizes, respectively, of set1 and set2. This is on the order of O(n) growth.
;;
