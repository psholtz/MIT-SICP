;;
;; Huffman codes are an effective technique for compressing data, savings of 20% to 90% are typical. 
;; The Huffman algorithm constructs a variable-length prefix-code based on a table of the frequencies
;; of occurrence of each character in a file, and builds up an optimal way of representing each character
;; as a (variable-length) binary string. Frequently-occurring characters are given short codes, while 
;; rarely-occurring characters are given long codes. The optimal code for a given file is always represented 
;; as a FULL binary tree, where every non-leaf node has two children. 
;;

;;
;; Procedures for generating the leaves of a Huffman tree:
;;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;;
;; Procedures for constructing and manipulating the Huffman tree:
;;
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else
	 (error "Bad bit -- CHOOSE BRANCH" bit))))

;;
;; Procedures for decoding messages using a Huffman tree:
;;
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch 
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;;
;; Procedures for encoding messages using a Huffman tree:
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else 
	 (element-of-set? x (cdr set)))))

;; (If your Scheme doesn't ship with a "reverse" procedure, this should work:
(define (reverse items)
  (define (reverse-iter lst1 lst2)
    (if (null? lst1)
	lst2
	(reverse-iter (cdr lst1) (cons (car lst1) lst2))))
  (reverse-iter items '()))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol-list encoded)
    (if (leaf? symbol-list)
	(reverse encoded)
	(let ((symbols-left (symbols (left-branch symbol-list)))
	      (symbols-right (symbols (right-branch symbol-list))))
	  (cond ((element-of-set? symbol symbols-left)
		 (encode-1 (left-branch symbol-list) (cons 0 encoded)))
		((element-of-set? symbol symbols-right)
		 (encode-1 (right-branch symbol-list) (cons 1 encoded)))
		(else
		 (error "Bad symbol: ENCODE-SYMBOL" symbol))))))
  (encode-1 tree '()))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;;
;; Procedures for generating the Huffman tree itself:
;;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else
	 (cons (car set)
	       (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ;; symbol
			       (cadr pair))  ;; frequency
		    (make-leaf-set (cdr pairs))))))

;;(define (accumulate op init seq)
;;  (if (null? seq)
;;      init
;;      (op (car seq)
;;	  (accumulate op init (cdr seq)))))

;;(define (successive-merge pairs)
;;  (define (successive-merge-lambda a b)
;;    (if (null? b)
;;	a
;;	(make-code-tree a b)))
;;  (accumulate successive-merge-lambda '() (reverse pairs)))

(define (successive-merge pairs)
  (if (= (length pairs) 1)
      (car pairs)
      (let ((first (car pairs))
	    (second (cadr pairs))
	    (rest (cddr pairs)))
	(successive-merge (adjoin-set (make-code-tree first second)
				      rest)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))