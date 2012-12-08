;;
;; Exercise 2.60
;;
;; We specified that a set would be represented as a list with no duplicates. Now suppose we 
;; allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2).
;; Design procedures "element-of-set?", "adjoin-set", "union-set", and "intersection-set" that 
;; operate on this representation. How does the efficiency of each compare with the corresponding
;; procedure for the non-duplicate representation? Are there applications for which you would 
;; use this representation in preference to the non-duplicate one?
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
;; Suppose next that we take the definition of "intersection-set" that was given in the 
;; text, and see whether it works for multisets:
;;
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
	((element-of-set? (car s1) s2)
	 (cons (car s1)
	       (intersection-set (cdr s1) s2)))
	(else
	 (intersection-set (cdr s1) s2))))

(intersection-set '(5 5) '(5 5 5))
;; ==> (5 5)

;;
;; Which is the correct answer.
;;
;; But now consider:
;;
(intersection-set '(5 5 5) '(5 5))
;; ==> (5 5 5)

;;
;; Which is, of course, the wrong answer.
;;
;; Expanding the call graph for "intersection-set", we can see why it's wrong:
;;
(intersection-set '(5 5 5) '(5 5))
(cons 5 (intersection-set '(5 5) '(5 5)))

;;
;; That second expression is already false, since (intersection-set '(5 5 5) '(5 5)) is NOT 
;; equivalent to (cons 5 (intersection-set '(5 5) '(5 5))). If we wish to use this same 
;; approach of iteratively recursing down set1, looking for matches in set2, we must 
;; remove from set2 all elements that we find in set1:
;;
(define (remove-element-set x set)
  (define (remove-element-set-iter working total)
    (cond ((null? working) total)
	  ((equal? x (car working))
	   (append total (cdr working)))
	  (else 
	   (remove-element-set-iter (cdr working) (adjoin-set (car working) total)))))
  (remove-element-set-iter set '()))

(remove-element-set 5 '(1 2 3))
;; ==> (3 2 1)
(remove-element-set 5 '(1 2 5))
;; ==> (2 1)
(remove-element-set 5 '(1 5 3))
;; ==> (1 3)
(remove-element-set 5 '(5 1 2))
;; ==> (1 2)
(remove-element-set 5 '(5 5 1))
;; ==> (5 1)
(remove-element-set 5 '(5 5 5))
;; ==> (5 5)

;;
;; This looks like just want need. We integrate it into "intersection-set" as follows:
;;
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
	((element-of-set? (car s1) s2)
	 (cons (car s1)
	       (intersection-set (cdr s1) (remove-element-set (car s1) s2))))
	(else 
	 (intersection-set (cdr s1) s2))))

;;
;; Let's see whether this new definition passes our use cases:
;;
(intersection-set '(5 5 5) '(5 5))
;; ==> (5 5)
(intersection-set '(5 5) '(5 5 5))
;; ==> (5 5)

;;
;; Looks good so far. A few more unit tests:
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
(intersection-set '(1 2 3) '(1))
;; ==> (1)
(intersection-set '(4) '(1 2 3))
;; ==> ()
(intersection-set '(1 2 3) '(4))
;; ==> ()
(intersection-set '(1 2 3) '(4 5 6))
;; ==> ()
(intersection-set '(4 5 6) '(1 2 3))
;; ==> ()

;;
;; To get a sense for how "intersection-set" performs, let's 
;; step through some call graphs, for both the old and new 
;; versions, to compare their respective performance.
;;
;; First, stepping through a call graph for the old version:
;;
(intersection-set '(1 2 3 4) '(3 4 5 6))
(if (element-of-set? 1 '(3 4 5 6))                     ;; element-of-set? takes 4 (i.e., N2) steps
    (cons 1 (intersection-set '(2 3 4) '(3 4 5 6)))
    (intersection-set '(2 3 4) '(3 4 5 6)))
(intersection-set '(2 3 4) '(3 4 5 6))        
(if (element-of-set? 2 '(3 4 5 6))                     ;; element-of-set? takes 4 (i.e,. N2) steps
    (cons 2 (intersection-set '(3 4) '(3 4 5 6)))
    (intersection-set '(3 4) '(3 4 5 6)))
(intersection-set '(3 4) '(3 4 5 6))
(if (element-of-set? 3 '(3 4 5 6))                     ;; element-of-set? takes 1 (but possible max of N2) steps
    (cons 3 (intersection-set '(4) '(3 4 5 6)))
    (inter-section-set '(4) '(3 4 5 6)))
(cons 3 (intersection-set '(4) '(3 4 5 6)))
(cons 3 (if (element-of-set? 4 '(3 4 5 6))             ;; element-of-set? takes 2 (but possible max of N2) steps
	        (cons 4 (intersection-set '() '(3 4 5 6)))
		    (intersection-set '() '(3 4 5 6))))
(cons 3 (cons 4 (intersection-set '() '(3 4 5 6))))
(cons 3 (cons 4 '()))
(cons 3 '(4))
'(3 4)

;;
;; Counting the steps, let n1 be the size of set1 and n2 be the size of set2.
;; Then the total number of steps taken is on the order of n1*n2 + n1, or O(n^2).
;;

;;
;; Now let's expand the same function call for the new
;; implementation of "intersection-set":
;;
(intersection-set '(1 2 3 4) '(3 4 5 6))
(if (element-of-set? 1 '(3 4 5 6))                     ;; element-of-set? takes 4 (i.e., N2) steps
    (cons 1 (intersection-set '(2 3 4) (remove-element-set 1 '(3 4 5 6))))
    (intersection-set '(2 3 4) '(3 4 5 6)))
(intersection-set '(2 3 4) '(3 4 5 6))
(if (element-of-set? 2 '(3 4 5 6))                     ;; element-of-set? takes 4 (i.e., N2) steps
    (cons 2 (intersection-set '(3 4) (remove-element-set 2 '(3 4 5 6))))
    (intersection-set '(3 4) '(3 4 5 6)))
(intersection-set '(3 4) '(3 4 5 6))
(if (element-of-set? 3 '(3 4 5 6))                     ;; element-of-set? takes 1 (but possible max of N2) steps
    (cons 3 (intersection-set '(4) (remove-element-set 3 '(3 4 5 6))))
    (intersection-set '(4) '(3 4 5 6)))                ;; remove-element-set takes max of N2 steps
(cons 3 (intersection-set '(4) (remove-element-set 3 '(3 4 5 6))))
(cons 3 (intersection-set '(4) '(4 5 6)))
(cons 3 (if (element-of-set? 4 '(4 5 6))               ;; element-of-set? takes 1 (but possible max of N2) steps
	    (cons 4 (intersection-set '() (remove-element-set 4 '(4 5 6))))
	    (intersection-set '() '(4 5 6))))          ;; remove-element-set takes max of N2 steps
(cons 3 (cons 4 (intersection-set '() (remove-element-set 4 '(4 5 6)))))
(cons 3 (cons 4 (intersection-set '() '(5 6))))
(cons 3 (cons 4 '()))
(cons 3 '(4))
'(3 4)

;;
;; The "outer loop", which steps through set1, will execute a total of 2*n1 times (where n1 is the 
;; size of set1, and n2 is the size of set2): there are n1 steps in "building" the recursion, and 
;; another n1 steps in "unwinding" it. At each step on the "way out", the procedure must invoke
;; "element-of-set?" to check whether the element is in the set. Let us consider the case where 
;; the target element is never in the set, and hence "remove-element-set" is never invoked. Since
;; "element-of-set?" runs in n2 steps, this means that in this case the intersection procedure 
;; completes in a total of n1*n2 + n1 steps, or again, on the order of O(n^2).
;;
;; Suppose now that the target element is found in the set (always), but only after all the entire
;; set2 has been searched. Suppose now also that remove-element must be called. This calculation is a 
;; little difficult to model, since set2 is decreasing in size at each step, the boundaries of its 
;; growth are determined in large part by the parameters. But let's suppose the following:
;;
;;  element-of-set? ==> n2 + (n2-1) + (n2-2) + ... + (n2-n1) 
;;  remove-element-set ==> n2 + (n2-1) + (n2-2) + .. + (n2-1)
;;
;; These two sums added together, together with the 2*n1 steps of the outer loop, total a number 
;; that is still on the order of O(n^2). 
;;
;; So in either case, performance of "intersection-set" is on the order of O(n^2), and if anything, 
;; the computation is slower and more involved in the multiset case than in the set case.
;; 

;;
;; Our previous definition of "union-set" deferred the "union"-ing to the 
;; the "adjoin-set" procedure, which we've modified accordingly. So in 
;; principle, we should be able to use the old "union-set" procedure.
;; However, since we're not checking for duplicates anymore, the expression
;; "union-set" just as succinctly using the "append" procedure:
;; 
(define (union-set s1 s2)
  (append s1 s2))

;;
;; Where append is defined as:
;;
;; (define (append lst1 lst2)
;;  (if (null? lst1)
;;      lst1
;;      (cons (car lst1)
;;            (append (cdr lst1) lst2))))
;;

;;
;; Let's expand a call graph, and compare to the call graph in the previous exercise:
;;
(union-set '(1 2 3) '(4 5 6))
(append '(1 2 3) '(4 5 6))
(cons 1 (append '(2 3) '(4 5 6)))
(cons 1 (cons 2 (append '(3) '(4 5 6))))
(cons 1 (cons 2 (cons 3 (append '() '(4 5 6)))))
(cons 1 (cons 2 (cons 3 '(4 5 6))))
(cons 1 (cons 2 '(3 4 5 6)))
(cons 1 '(2 3 4 5 6))
'(1 2 3 4 5 6)

;;
;; Subtracting out the calls to "adjoin-set", which in the previous exercise was a O(n)
;; operation, we see that now the entire "union-set" procedure executes in linear time.
;;

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
;; ==> (1 1 2 3)
(union-set '(4) '(1 2 3))
;; ==> (4 1 2 3)
(union-set '(1 2 3) '(4 5 6))
;; ==> (1 2 3 4 5 6)

;;
;; Some specific multiset examples:
;;
(union-set '(5 5) '(5 5 5))
;; ==> (5 5 5 5 5)
(union-set '(5 5 5) '(5 5))
;; ==> (5 5 5 5 5)
(intersection-set '(5 5) '(5 5 5))
;; ==> (5 5)
(intersection-set '(5 5 5) '(5 5))
;; ==> (5 5)

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
;;  intersection-set ==> 

;;As indicated above, the running time for "intersect-set" is O(n^2) 
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
;; We can build the following table:
;;
;;  +--------------------+----------+-------------+
;;  |  PROCEDURE         |  SET     |  MULTI-SET  |    
;;  +--------------------+----------+-------------+
;;  |  element-of-set?   |  O(n)    |  O(n)       |
;;  |  adjoin-set        |  O(n)    |  constant   |
;;  |  intersection-set  |  O(n^2)  |  O(n^2)     |
;;  |  union-set         |  O(n^2)  |  O(n)       |
;;  +--------------------+----------+-------------+
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
  ;;
  ;; "union-set" and "intersection-set" are already 
  ;; adapted to handle multisets, so we can use them:
  ;;
  (let ((n1 (length (intersection-set s1 s2)))
	(n2 (length (union-set s1 s2))))
    ;;
    ;; Multiply by 1.0 to ensure casting to double:
    ;;
    (/ (* 1.0 n1) (* 1.0 n2))))
 
(define s1 '(a a a b))
(define s2 '(a a b b c))

(intersection-set s1 s2)
;; ==> '(a a b)
(union-set s1 s2)
;; ==> '(a a a b a a b b c)
(jaccard s1 s2)
;; ==> 0.333333333333

(define t1 '(1 2 3 4))
(define t2 '(2 3 5 7))
(define t3 '(2 4 6))

(intersection-set t1 t2)
;; ==> (2 3)
(union-set t1 t2)
;; ==> (1 2 3 4 2 3 5 7)
(jaccard t1 t2)
;; ==> 0.25

(intersection-set t2 t3)
;; ==> (2)
(union-set t2 t3)
;; ==> (2 3 5 7 2 4 6)
(jaccard t2 t3)
;; ==> 0.142857...

(intersection-set t1 t3)
;; ==> (2 4)
(union-set t1 t3)
;; ==> (1 2 3 4 2 4 6)
(jaccard t1 t3)
;; ==> 0.285714...

(define u1 '(1 1 1 2))
(define u2 '(1 1 2 2 3))
(define u3 '(1 2 3 4))

(intersection-set u1 u2)
;; ==> (1 1 2)
(union-set u1 u2)
;; ==> (1 1 1 2 1 1 2 2 3)
(jaccard u1 u2)
;; ==> 0.33333333333

(intersection-set u2 u3)
;; ==> (1 2 3)
(union-set u2 u3)
;; ==> (1 1 2 2 3 1 2 3 4)
(jaccard u2 u3)
;; ==> 0.33333333333

(intersection-set u1 u3)
;; ==> (1 2)
(union-set u1 u3)
;; ==> (1 1 1 2 1 2 3 4)
(jaccard u1 u3)
;; ==> 0.25

;;
;; See Prof. Ullman's Stanford CS345 course on Data Mining for more information.
;;