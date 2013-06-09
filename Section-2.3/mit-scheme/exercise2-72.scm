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
	  (let ((tree-left (symbols left))
		(tree-right (symbols right)))
	    (cond ((element-of-set? symbol tree-left)
		   (encode-1 left (cons 0 encoded)))
		  ((element-of-set? symbol tree-right)
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
;; We will consider only the special case described above, where n = 2^k.
;; 
;; For the case k = 4, the tree will look like this:
;;
(define tree (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8))))

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
(encode-1 '((a 1) (b 2) (c 4) (d 8)) '())

;;
;; Calls are now made to:
;;
;;  1. (leaf? '((a 1) (b 2) (c 4) (d 8)))
;;
;; and then:
;;
;;  2. (left-branch '((a 1) (b 2) (c 4) (d 8)))
;;  3. (right-branch '((a 1) (b 2) (c 4) (d 8)))
;; 
;; First the "left-branch" is searched, which really only has one element in it.
;; 
;; The target element is identified in this branch. We "cons" 0 onto "total", and 
;; then make the recursive call:
;;
(encode-symbol-iter '(leaf d 8) '(0))

;;
;; Since this is a leaf, we terminate the recursion here.
;;
;; "reverse" is applied to the list '(0), but since it too has only 1 element, 
;; the procedure runs in constant time.
;;

;;
;; There are several sub-procedures which are invoked, which, in the general 
;; case would run in O(n) time, but given the nature of the data structures
;; we're using, there is no invocation here that does not complete in more
;; than constant time.
;;
;; Hence, no matter how large n is, the encoding of the most frequent symbol
;; will always terminate in constant time. 
;;

;;
;; For the least frequent symbol, we again enter the call graph, and invoke
;; the procedures "leaf?", "left-branch", "right-branch", all of which execute
;; in linear time. We must then search for the target symbol in the list of 
;; symbols, both in the "left" branch, which always has just 1 element and terminates
;; in constant time, and the "right" branch, which in the first iteration will have 
;; (n-1) elements (and so runs in O(n) time).
;; 
;; The same sequence repeats itself (mainly constant-time operations), except 
;; again for searching the symbol list, which now has (n-2) elements in it. 
;; Note too, that in this implementation, the target symbol we are seeking (i.e., 
;; the least frequency symbol), appears at the end of the symbol list, so the 
;; entire list must be searched before the symbol is found:
;;
(symbols (right-branch tree))
;; ==> (c b a)

;;
;; In total, the total number of steps that will be performed when searching
;; the symbol set, via "element-of-set?", will add up to:
;;
;;  (n-1) + (n-2) + ... + 1
;;
;; This will total (1/2) * (n-1) * (n-2), or, in other words, be O(n^2).
;;
;; Note too that once we have generated our symbol list, which has been 
;; build up using constant-time "cons" operations, we must reverse it, which 
;; requires one application of a linear-time procedure. Still, the O(n^2)
;; time of the symbol search predominates this operation.
;;
;; In summary:
;;
;; (1) To find the most frequent symbol requires constant time.
;;
;; (2) To find the least frequent symbol requires O(n^2) time.
;; 
