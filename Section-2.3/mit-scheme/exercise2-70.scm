;;
;; Exercise 2.70
;;
;; The following eight-symbol alphabet with associated relative frequencies was designed to 
;; efficiently encode the lyrics of 1950s rock songs. (Note that the "symbols" of an "alphabet"
;; need not be individual letters.)
;; 
;;  A 2     NA 16
;;  BOOM 1  SHA 3
;;  GET 2   YIP 9
;;  JOB 2   WAH 1
;;
;; Use "generate-huffman-tree" (Exercise 2.69) to generate a corresponding Huffman tree, and use
;; "encode" (Exercise 2.68) to encode the following message:
;;
;;  Get a job
;;  Sha na na na na na na na na
;;  Get a job
;;  Sha na na na na na na na na
;;  Wah yip yip yip yip yip yip yip yip yip
;;  Sha boom
;;
;; How many bits aer required for the encoding? What is the smallest number of bits that would be 
;; needed to encode this song if we used a fixed-length code for the 8-symbol alphabet?
;;

(load "huffman.scm")

;;
;; With the supporting procedures defined, this exercise is pretty straightforward:
;;
(define tree (generate-huffman-tree
	      '((A 2)
		(BOOM 1)
		(GET 2)
		(JOB 2)
		(NA 16)
		(SHA 3)
		(YIP 9)
		(WAH 1))))

(symbols tree)
;; ==> (na yip a wah boom sha job get)
(weight tree)
;; ==> 36
(+ 2 1 2 2 16 3 9 1)
;; ==> 36

;;
;; The Huffman tree looks something like this:
;;
;;      (root)
;;        / \ 
;;       /   \
;;      /     \
;;  (na 16)   ()
;;           /  \
;;          /    \
;;         /      \
;;     (yip 9)    ()
;;               /  \
;;              /    \
;;             /      \
;;            /       ()
;;           /        / \ 
;;          /        /   \
;;         /        /     \
;;        /    (sha 3)   () 
;;       /                / \
;;      ()               /   \
;;     /  \             /     \
;;    /    \        (job 2) (get 2)
;;   /      \
;; (a 2)    ()
;;         /  \
;;        /    \
;;       /      \
;;    (wah 1)  (boom 1)
;;
;; Clearly, the leaves with larger weights are sorted 
;; closer to the root of the tree.
;;

(encode '(get a job) tree)
;; ==> (1 1 1 1 1 1 1 0 0 1 1 1 1 0)
(encode '(sha na na na na na na na na) tree)
;; ==> (1 1 1 0 0 0 0 0 0 0 0 0)
(encode '(get a job) tree)
;; ==> (1 1 1 1 1 1 1 0 0 1 1 1 1 0)
(encode '(sha ha na na na na na na na) tree)
;; ==> (1 1 1 0 0 0 0 0 0 0 0 0)
(encode '(wah yip yip yip yip yip yip yip yip yip) tree)
;; ==> (1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
(encode '(sha boom) tree)
;; ==> (1 1 1 0 1 1 0 1 1)

;;
;; Let's see if these decode correctly:
;;
(decode '(1 1 1 1 1 1 1 0 0 1 1 1 1 0) tree)
;; ==> (get a job)
(decode '(1 1 1 0 0 0 0 0 0 0 0 0) tree)
;; ==> (sha na na na na na na na na)
(decode '(1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0) tree)
;; ==> (wah yip yip yip yip yip yip yip yip yip)
(decode '(1 1 1 0 1 1 0 1 1) tree)
;; ==> (sha boom)

;;
;; How many bits are required by this encoding?
;;
(* 2 (length (encode '(get a job) tree)))
;; ==> 28
(* 2 (length (encode '(sha na na na na na na na na) tree)))
;; ==> 24
(length (encode '(wah yip yip yip yip yip yip yip yip yip) tree))
;; ==> 23
(length (encode '(sha boom) tree))
;; ==> 9

(+ 28 24 23 9)
;; ==> 84

;;
;; So 84 bits are required using the Huffman encoding.
;;

;;
;; Let's calculate how much space is required if we used a fixed-length 
;; encoding of 3 bits:
;;
;;  '(get a job) ==> 3 symbols ==> 9 bits
;;  '(sha na na na na na na na na) ==> 9 symbols ==> 27 bits
;;  '(wah yip yip yip yip yip yip yip yip yip) ==> 10 symbols ==> 30 bits
;;  '(sha boom) ==> 2 symbols ==> 6 bits
;;
(+ (* 2 (+ 9 27)) 30 6)
;; ==> 108

;;
;; If we used fixed-length encoding, we would require 108 bits.
;;
;; The Huffman encoding scheme requires only about 77.8% of the space that is 
;; required by fixed length encoding.
;;