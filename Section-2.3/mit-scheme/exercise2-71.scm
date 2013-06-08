;;
;; Exercise 2.71
;;
;; Suppose we have a Huffman tree for an alphabet of "n" symbols, and that the relative frequencies 
;; of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the tree for n=5 and n=10. In such a tree (for 
;; general n) how many bits are required to encode the most frequent symbol? The last frequency symbol?
;;

(load "huffman.scm")

(define tree1 (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16))))

(encode-symbol 'a tree1)
;; ==> (1 1 1 1)
(encode-symbol 'b tree1)
;; ==> (1 1 1 0)
(encode-symbol 'c tree1)
;; ==> (1 1 0)
(encode-symbol 'd tree1)
;; ==> (1 0)
(encode-symbol 'e tree1)
;; ==> (0)

;;
;; The tree looks like this:
;;
;;               ()
;;              /  \
;;             /    \    
;;            ()  (e 16)
;;           /  \
;;          /    \
;;         ()   (d 8) 
;;        /  \
;;       /    \
;;      ()   (c 4) 
;;     /  \
;;    /    \ 
;; (a 1)  (b 2)
;; 

;;
;; It's interesting to trace through the call graph of "generate-huffman-tree" to see how the tree is formed.
;;

;;
;; We call "make-leaf-set" on the set of pairs, to generate a makeshift "priority queue" of leaves:
;;
(make-leaf-set '((a 1) (b 2) (c 4) (d 8) (e 16)))
;; ==> ((leaf a 1) (leaf b 2) (leaf c 4) (leaf d 8) (leaf e 16))

;;
;; This makeshift priority queue is then passed to the "successive-merge" procedure:
;;
(successive-merge '((leaf a 1) (leaf b 2) (leaf c 4) (leaf d 8) (leaf e 16)))

;;
;; Recall the definition of "successive-merge":
;;
(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (let ((first (car pairs))
	    (second (cadr pairs))
	    (rest (cddr pairs)))
	(successive-merge (adjoin-set (make-code-tree first second)
				      rest)))))

;;
;; Evaluating the formation of the Huffman tree:
;;
(successive-merge '((leaf a 1) (leaf b 2) (leaf c 4) (leaf d 8) (leaf e 16)))
;; first <- (leaf a 1)
;; second <- (leaf b 2)
;; rest <- ((leaf c 4) (leaf d 8) (leaf e 16))
(successive-merge '(((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (leaf d 8) (leaf e 16)))

;;
;; After the first iteration, we can see that when the two smallest leaves are combined (i.e., Leaf A with 
;; weight 1, and Leaf B with weight 2), the combined weight of the new combination is only 3, which is one
;; less than the next leaf (i.e., Leaf C with weight 4). This pattern will repeat until the procedure
;; terminates, that is, the result of each successive combination will be a combined leaf whose accumulated
;; weight is just one short of the weight of the next leaf in the (priority) queue. 
;;
;; Consequently, the structure of the Huffman tree structure will always be similar to that given above.
;;
;; Finishing the evaluation:
;; 
(successive-merge '((((leaf a1) (leaf b2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (leaf e 16)))
(successive-merge '(((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16)))
(successive-merge '((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31)))
;; ==> ((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7) (leaf d 8) (a b c d) 15) (leaf e 16) (a b c d e) 31))

;;
;; At each iteration, the combined weight of the smallest node is 1 less than the weight of the next leaf
;; in the priority queue.
;;

;;
;; For a general tree of size n, (n-1) bits are required to encode the least frequent symbol, and 1 bit is 
;; required to encode the most frequent symbol.
;;