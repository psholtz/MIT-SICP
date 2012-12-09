;;
;; Exercise 2.61
;;
;; Give an implementation of "adjoin-set" using the ordered representation. By analogy with "element-of-set?"
;; how to take advantage of the ordering to produce a procedure that requires on the average abotu half as 
;; many steps as with the unordered representation.
;;

;;
;; NOTE: We implement here "sets", that is, repeated elements are not allowed (i.e., we are not
;; implementing multisets here).
;;

;;
;; We give a recursive definition:
;;
(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (let ((head-element (car set)))
	(cond ((< x head-element) (cons x set))
	      ((= x head-element) set)
	      (else
	       (cons head-element (adjoin-set x (cdr set))))))))

;;
;; Some unit tests:
;;
(adjoin-set 1 '())
;; ==> (1)

(define x '(1 3 5 7))

(adjoin-set 0 x)
;; ==> (0 1 3 5 7)
(adjoin-set 1 x)
;; ==> (1 3 5 7)
(adjoin-set 2 x)
;; ==> (1 2 3 5 7)
(adjoin-set 3 x)
;; ==> (1 3 5 7)
(adjoin-set 4 x)
;; ==> (1 3 4 5 7)
(adjoin-set 5 x)
;; ==> (1 3 5 7) 
(adjoin-set 6 x)
;; ==> (1 3 5 6 7)
(adjoin-set 7 x)
;; ==> (1 3 5 7)
(adjoin-set 8 x)
;; ==> (1 3 5 7 8)

;;
;; Let's expand a call-graph, to get a sense for the performance of the procedure in time/space:
;;
(adjoin-set 4 '(1 3 5 7))
(cons 1 (adjoin-set 4 '(3 5 7)))
(cons 1 (cons 3 (adjoin-set 4 '(5 7))))
(cons 1 (cons 3 '(4 5 7)))
(cons 1 '(3 4 5 7))
'(1 3 4 5 7)

;;
;; The procedure takes (at most) on the order of 2*n steps in time, and requires (at most)
;; roughly n slots in space/memory, so the procedure is O(n) in both time and space. In general, 
;; though, the procedure will execute much more rapidly than this, on average only requiring 
;; about n/2 time steps.
;;

;;
;; We can also give an iterative definition as follows:
;;
(define (adjoin-set x set)
  (define (adjoin-set-iter prefix postfix)
    (if (null? postfix)
	(append prefix (cons x postfix))
	(let ((head-element (car postfix)))
	  (cond ((< x head-element) (append prefix (cons x postfix)))
		((= x head-element) (append prefix postfix))
		(else 
		 (adjoin-set-iter (append prefix (list head-element)) (cdr postfix)))))))
  (adjoin-set-iter '() set))

;;
;; This procedure passes the same unit tests as those defined above.
;;

;;
;; Let's expand a call-graph for the iterative procedure:
;;
;; First, recall that the definition of "append" is given as:
;;
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; 
;; We have:
;;
(adjoin-set 4 '(1 3 5 7))
(adjoin-set-iter '() '(1 3 5 7))
(adjoin-set-iter (append '() '(1)) '(3 5 7))
(adjoin-set-iter (append (append '() '(1)) '(3)) '(5 7))
(append (append (append '() '(1)) '(3)) (cons 4 '(5 7)))
(append (append (append '() '(1)) '(3)) '(4 5 7))
(append (append '(1) '(3)) '(4 5 7))
(append (cons 1 (append '() '(3))) '(4 5 7))
(append (cons 1 '(3)) '(4 5 7))
(append '(1 3) '(4 5 7))
(cons 1 (append '(3) '(4 5 7)))
(cons 1 (cons 3 (append '() '(4 5 7))))
(cons 1 (cons 3 '(4 5 7)))
(cons 1 '(3 4 5 7))
'(1 3 4 5 7)

;;
;; "append" is a slightly messy operation in terms of being O(n). Calling "append"
;; recursively like this is probably not the most efficient implementation. The 
;; first-given definition of "adjoin-set", the recursive definition, is in this
;; case probably the most efficient.
;;

;;
;; Let's compare this to the call graph of the original "adjoin-set", which implemented
;; the "unordered" sets and which made a call at each step to "element-of-set?":
;;
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else
	 (element-of-set? x (cdr set)))))

;;
;; Original "unordered" definition for "adjoin-set":
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;
;; Let's expand the same call graph:
;;
(adjoin-set 4 '(1 3 5 7))
(if (element-of-set? 4 '(1 3 5 7))
    '(1 3 5 7)
    (cons 4 '(1 3 5 7)))
'(4 1 3 5 7)

;;
;; Expanding the call to "element-of-set?":
;;
(element-of-set? 4 '(1 3 5 7))
(element-of-set? 4 '(3 5 7))
(element-of-set? 4 '(5 7))
(element-of-set? 4 '(7))
(element-of-set? 4 '())
#f

;;
;; We can see that the total number of time steps required here is 3 + 6 = 9, 
;; while the total number of time steps required for the new definition of 
;; "adjoin-set" is only 6. This is about 2/3 the execution time, although the 
;; set size here is very small and it's not hard to see that in the average 
;; case, the running time of the new procedure will be about 50% the time of 
;; the old, unordered procedure.
;;