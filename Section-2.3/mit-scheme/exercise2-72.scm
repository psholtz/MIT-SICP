;;
;; Exercise 2.72
;;
;; Consider the encoding procedure that you designed in exercise 2.68. What is the order of growth in 
;; the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search 
;; the symbol list at each node encountered. To answer this question in general is difficult. Consider the 
;; special case where the relative frequencies of the n symbols are as described in exercise 2.71, and give 
;; the order of growth (as a function of n) of the number of steps needed to encode the most frequent and 
;; least frequent symbols in the alphabet.
;;

(load "huffman.scm")

;;
;; Let's look more closely at the "encode-symbol" procedure:
;;
(define (encode-symbol symbol tree)
  (define (encode-1 tree-list encoded)
    (if (leaf? tree-list)
	(reverse encoded)
	(let ((left (left-branch tree-list))
	      (right (right-branch tree-list)))
	  (let ((symbols-left (symbols left))
		(symbols-right (symbols right)))
	    (cond ((element-of-set? symbol symbols-left)
		   (encode-1 left (cons 0 encoded)))
		  ((element-of-set? symbol symbols-right)
		   (encode-1 right (cons 1 encoded)))
		  (else
		   (error "Bad symbol: ENCODE-SYMBOL" symbol)))))))
  (encode-1 tree '()))

;;
;; Let's also look at some of the other required procedures:
;;
(define (left-branch tree (car tree)))
(define (right-branch tree) (cadr tree))
(define (leaf? object) (eq? (car object) 'leaf))

;;
;; Each of these three supporting procedures runs in constant time.
;;

;;
;; Let's also repost the definition of the "symbols" procedure:
;;
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

;;
;; This procedure also runs in constant time.
;;

;;
;; Finally let's look at the "element-of-set?" procedure:
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else
	 (element-of-set? x (cdr set)))))

;;
;; This procedure runs in O(N) time, where N is the length of the argument set.
;;

;;
;; We will consider only the special case described above, where n = 2^k.
;; 
;; For the case k = 4, the tree will look like this:
;;
(define tree (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8))))
;; ==> ((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15)

(encode-symbol 'a tree)
;; ==> (0 0 0)
(encode-symbol 'b tree)
;; ==> (0 0 1) 
(encode-symbol 'c tree)
;; ==> (0 1)
(encode-symbol 'd tree)
;; ==> (1)

;;
;; The tree will look something like the following:
;;
;;            (root)
;;             /  \ 
;;            /    \ 
;;           ()   (d 8)
;;          /  \ 
;;         /    \ 
;;        ()   (c 4)
;;       /  \ 
;;      /    \
;;   (a 1)   (b 2)
;;

;;
;; What is the running time to encode the most frequent symbol?
;;  
;; Let's step through the call graph:
;;
(encode-symbol 'd tree)
(encode-1 tree '())

;;
;; The first call is made to:
;; 
;;   (leaf? tree-list)
;;
;; This procedure executes in constant time.
;;

;;
;; The next two calls are made to:
;;
;;   1. (left-branch tree-list)
;;   2. (right-branch tree-list)
;;
;; Both of these also execute in constant time.
;;

;;
;; The next two calls are made to:
;;
;;   1. (symbols left)
;;   2. (symbols right)
;;
;; As indicated above, the "symbols" procedure runs in constant time.
;;

;;
;; Finally, there is a conditional expression with two branches:
;;
;;   1. (element-of-set? symbol symbols-left)
;;   2. (element-of-set? symbol symbols-right)
;;
;; The conditional branches are evaluated in order. 
;;
;; First the left branch is searched. As we indicated above, this procedure
;; runs in O(N) time, where N is the length of the "symbols-left" list. In this
;; case, the "symbols-left" structure is of size N-1, but in terms of order 
;; of growth, we can classify this as O(N).
;;
;; The first conditional evaluation fails, and so the second expression in the 
;; conditional expression is evaluated. Again, in general, this procedure will 
;; run in O(N) time, where N is the size of the "symbols-right" list. However, 
;; owing to the particular structure of this problem, "symbols-right" will always
;; have only 1 element in it (given that we are encoding the most frequently 
;; occurring symbol). Hence, we can surmise that this branch runs in O(1) time.
;;
;; The total time to encode the most frequent symbol is O(N) + O(1), or O(N) time.
;;

;;
;; To evaluate the running time for the last frequently occurring symbol, 
;; consider that we arrive at the evaluation of the conditional expression, as above.
;; The two branches to evaluate, using our example based on N=4, would looks like:
;;
;;   1. (element-of-set? 'a '(a b c))
;;   2. (element-of-set? 'a '(d))
;;
;; The first expression evaluates to true, which results in a recursive call
;; to "encode-1". The next time we hit the evaluation of this same conditional 
;; expression, the two branches look like:
;;
;;   1. (element-of-set? 'a '(a b))
;;   2. (element-of-set? 'a '(c))
;;
;; Again, the first expression evaluates to true, which results in a recursive call
;; to "encode-1". The next time we hit the evaluation of this same conditional
;; expression, the two branches look like:
;;
;;   1. (element-of-set? 'a '(a))
;;   2. (element-of-set? 'a '(b))
;;
;; Again, the first expression evaluates to true, which results in a recursive call
;; to "encode-1", at which point the recursion terminates.
;;

;;
;; The "element-of-set?" procedure runs in time proportional to the length of the size 
;; of the argument set. The procedure is invoked a total of of (N-1) times, but we 
;; time we invoke the procedure, we are passing in a list that is one element shorter 
;; than before. Hence, the running time of the recursion looks like:
;;
;;   T(N) = (N-1) + (N-2) + .. + 1
;;
;;   T(N) = (1/2) * N * (N-1)
;;
;;   T(N) = O(N^2)
;;
;; The total time to encode the least frequent symbol is O(N^2).
;;

;;
;; Note too that once we have generated our symbol list, which has been build up using 
;; constant-time "cons" operations, we must reverse it, which requires one application 
;; of a linear-time "reverse" procedure. Still, the O(n^2) time of the symbol search 
;; predominates this operation.
;;
;; In summary:
;;
;; (1) To encode the most frequent symbol requires O(n) time.
;;
;; (2) To encode the least frequent symbol requires O(n^2) time.
;; 
