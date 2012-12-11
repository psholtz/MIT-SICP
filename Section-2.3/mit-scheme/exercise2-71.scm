;;
;; Exercise 2.71
;;
;; [WORKING]
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

