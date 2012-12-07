;;
;; Exercise 2.60
;;
;; [Working]
;;

;;
;; Sets which allow for duplicates are often referred to as "bags" or "multisets".
;;

;;
;; "element-of-set?" does not need to be modified: it stills performs a linear walk down the 
;; list structure, terminating either (a) when the target element has been found; or (b) 
;; when the end of the list structure has been reached.
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else
	 (element-of-set? x (cdr set)))))

;;
;; "adjoin-set" will be modified, and is now much simpler since we do not need to check 
;; for set membership prior to "cons"-ing the new element into the set:
;;
(define (adjoin-set x set)
  (cons x set))

;;
;; It turns out that neither "intersection-set" nor "union-set" need to be modified.
;;
;; We can still use both these procedures as before:
;;
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
	((element-of-set? (car s1) s2)
	 (cons (car s1)
	       (intersection-set (cdr s1) s2)))
	(else
	 (intersection-set (cdr s1) s2))))

;;
;; We'll use the "second" definition of "union-set" given in Exercise 2.59, since it 
;; leaves the elements of the joined set listed in a more natural order:
;;
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))

(define (union-set s1 s2)
  (accumulate (lambda (a b) (adjoin-set a b)) s1 s2))

;;
;; Let's first run through the old use cases, to make sure they behave as we expect:
;;
(union-set '() '())
;; ==> ()
(union-set '(1 2 3) '())
;; ==> (1 2 3)
(union-set '() '(1 2 3))
;; ==> (1 2 3)
(union-set '(1 2 3) '(1 2 3))
;; ==> (1 2 3 1 2 3)
(union-set '(1) '(1 2 3))
;; ==> (1 2 3 1)
(union-set '(4) '(1 2 3))
;; ==> (1 2 3 4)
(union-set '(1 2 3) '(4 5 6))
;; ==> (4 5 6 1 2 3)

;;
;; Let's run the same tests on intersection:
;;
(intersection-set '() '())
;; ==> ()
(intersection-set '(1 2 3) '())
;; ==> ()
(intersection-set '() '(1 2 3))
;; ==> ()
(intersection-set '(1 2 3) '(1 2 3))
;; ==> (1 2 3)
(intersection-set '(1) '(1 2 3))
;; ==> (1)
(intersection-set '(4) '(1 2 3))
;; ==> ()
(intersection-set '(1 2 3) '(4 5 6))
;; ==> ()

;;
;; Some specific multiset examples:
;;
(union-set '(5 5) '(5 5 5))
;; ==> (5 5 5 5 5)
(intersection-set '(5 5) '(5 5 5))
;; ==> (5 5)

;;
;; Let's step through a call-graph for "intersection-set", to get a 
;; sense for the performance of this procedure:
;;
(intersection-set '(1 2 3 4) '(3 4 5 6))
(if (element-of-set? 1 '(3 4 5 6))
    (cons 1 (intersection-set '(2 3 4) '(3 4 5 6)))
    (intersection-set '(2 3 4) '(3 4 5 6)))
(intersection-set '(2 3 4) '(3 4 5 6))
(if (element-of-set? 2 '(3 4 5 6))
    (cons 2 (intersection-set '(3 4) '(3 4 5 6)))
    (intersection-set '(3 4) '(3 4 5 6)))
(intersection-set '(3 4) '(3 4 5 6))
(if (element-of-set? 3 '(3 4 5 6))
    (cons 3 (intersection-set '(4) '(3 4 5 6)))
    (inter-section-set '(4) '(3 4 5 6)))
(cons 3 (intersection-set '(4) '(3 4 5 6)))
(cons 3 (if (element-of-set? 4 '(3 4 5 6))
	    (cons 4 (intersection-set '() '(3 4 5 6)))
	    (intersection-set '() '(3 4 5 6))))
(cons 3 (cons 4 (intersection-set '() '(3 4 5 6))))
(cons 3 (cons 4 '()))
(cons 3 '(4))
'(3 4)

;;
;; The algorithm must make a linear walk down each element of set1 (if the size of 
;; set1 is n1, there are n1 steps here), and at each step it must check whether the 
;; element is in set1 (if the size of set2 is n2, this could require as many as n2 
;; steps). Hence, the total running time is n1*n2, or O(n^2).
;;	     

;;
;; In terms of performance:
;;
;;  element-of-set? ==> This procedure did not change. It is O(n) for both sets and multisets.
;;
;;  adjoin-set ==> For sets, adjoin-set made a call to element-of-set?, which ran in O(n) time. 
;;                 For multisets, no such call is required, and hence it runs the constant time
;;                 necessary to perform a "cons". Hence, this procedure is O(n) for sets, and 
;;                 O(k), or constant-time for multisets. In other words, it runs much faster 
;;                 for multisets.
;; 
;;  intersection-set ==> As indicated above, the running time for "intersect-set" is O(n^2) 
;;                       for sets. Nothing changes in this procedure in the case of multisets, 
;;                       so the running time is still O(n^2). Hence, the running time for 
;;                       "intersection-set" is O(n^2) for sets, and O(n^2) for multisets.
;;
;;  union-set ==> We walked through the call graph for the set procedure in the previous
;;                problem. Let n1 be the size of set1, and n2 be the size of set2. There 
;;                we showed that union-set invokes 2*n1 calls to "unwind" set1 (i.e., 
;;                (1) 1 call to schedule an "adjoin-set" procedure; and (2) 1 call to 
;;                actually "cons" the next "car" to the list. In addition, though, to enforce
;;                the uniqueness of sets, calls to "element-of-set?" must be made prior to 
;;                each cons. The time required to perform this check grows as the size of the 
;;                constructed union grows. The first such call requires n2 steps to check the 
;;                size of set2. The last such call requires n2+n1 steps to check the size of 
;;                the generated union. there are, in all, n1 such calls. In other words, the 
;;                calls to "element-of-sets?" require a total of:
;;
;;                   n2 + (n2+1) + (n2+2) + ... + (n2+n1) 
;; 
;;                steps. A lower bound for this summation is n1*n2 (although, in fact, this 
;;                number is substantially larger than this. As a "lower bound", then we can 
;;                anticipate that a call to union-set, for "sets", will require 2n1 + n1*n2
;;                steps. In other words, the procedure runs in O(n^2) time.
;;
;;                For multisets, no such time-consuming check to "element-of-set?" is required.
;;                Hence, only 2*n1 steps are required, and the procedure completes in linear, 
;;                or O(n) time.
;;
;;                Hence, for sets, the procedure runs in O(n^2) time, while for multisets, 
;;                the procedure runs in O(n) time (a substnatial performance improvement).
;;

;;
;; Sets are a very common mathematical construct and are used widely throughout 
;; computer science. For instance, a relational database is designed to return 
;; a matching "set" of tuples in response to SQL queries.
;;

;;
;; Bags, or multisets, are of more specialized application. One area where bags
;; are used is in data mining applications, where they are used to calculate the 
;; Jaccard similarity between two (multi)sets of data. Formerly, the Jaccard
;; index of two sets is defined as the ratio of the size of the intersection 
;; of the sets to the size of the union of the two sets:
;;
;;  J(A,B) = |A n B| / |A u B|
;;
;; Because of the nature of data mining, it is more convenient to allow the sets 
;; A and B to be multisets, or bags allowing for repetitions of the same element.
;;

;;
;; We could, for instance, define a procedure to calculate the Jaccard index 
;; of two multisets A and B:
;;
(define (jaccard s1 s2)
  '())

;; [WORKING]

;;
;; See Prof. Ullman's Stanford CS345 course on Data Mining for more information.
;;