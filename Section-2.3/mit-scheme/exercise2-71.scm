;;
;; Exercise 2.71
;;
;; Suppose we have a Huffman tree for an alphabet of "n" symbols, and that the relative frequencies 
;; of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the tree for n=5 and n=10. In such a tree (for 
;; general n) how many bits are required to encode the most frequent symbol? The last frequency symbol?
;;

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
;;     (root) 
;;      /  \
;;     /    \
;;  (e 16)  () 
;;         /  \
;;        /    \
;;     (d 8)   () 
;;            /  \
;;           /    \
;;        (c 4)   ()
;;               /  \
;;              /    \
;;           (b 2)  (a 1)
;; 

(define tree2 (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512))))

(encode-symbol 'a tree2)
;; ==> (1 1 1 1 1 1 1 1 1)
(encode-symbol 'b tree2)
;; ==> (1 1 1 1 1 1 1 1 0)
(encode-symbol 'c tree2)
;; ==> (1 1 1 1 1 1 1 0)
(encode-symbol 'd tree2)
;; ==> (1 1 1 1 1 1 0)
(encode-symbol 'e tree2)
;; ==> (1 1 1 1 1 0)
(encode-symbol 'f tree2)
;; ==> (1 1 1 1 0)
(encode-symbol 'g tree2)
;; ==> (1 1 1 0)
(encode-symbol 'h tree2)
;; ==> (1 1 0)
(encode-symbol 'i tree2)
;; ==> (1 0)
(encode-symbol 'j tree2)
;; ==> (0)

;;
;; The tree looks like this:
;;
;;     (root) 
;;      /  \
;;     /    \
;; (j 512)  () 
;;         /  \
;;        /    \
;;    (i 256)  () 
;;            /  \
;;           /    \
;;       (h 128)  ()
;;               /  \
;;              /    \
;;          (g 64)   () 
;;                  /  \
;;                 /    \
;;              (f 32)  ()
;;                     /  \
;;                    /    \
;;                 (e 16)  ()
;;                        /  \
;;                       /    \
;;                    (d 8)   () 
;;                           /  \
;;                          /    \
;;                       (c 4)   ()
;;                              /  \
;;                             /    \
;;                          (b 2)  (a 1)
;;

;;
;; For a general tree of size n, (n-1) bits are required to encode the 
;; least frequent symbol, and 1 bit is required to encode the most 
;; frequent symbol.
;;