;;
;; Exercise 2.68
;;
;; The "encode" procedure takes as arguments a message and a tree and produces the list of bits that 
;; gives the encoded message.
;;
;;  (define (encode message tree)
;    (if (null? message)
;;       '()
;;       (append (encode-symbol (car message) tree)
;;               (encode (cdr message) tree))))
;;
;; "Encode-symbol" is a procedure, which you must write, that returns the list of bits that encodes 
;; a given symbol according to a given tree. You should design encode-symbol so that it signals an 
;; error if the symbol is not in the tree at all. Test your procedure by encoding the result you 
;; obtained in exercise 2.67 with the sample tree and seeing whether it is the same as the original 
;; sample message.
;;

;;
;; First let's reimport all the same tree code we used before:
;;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;;
;; Procedures for decoding symbols:
;;
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else
	  (error "Bad bit -- CHOOSE BRANCH" bit))))

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
;; Now define the "encode" procedure. 
;;
;; First we define some of the supporting procedures:
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else 
	 (element-of-set? x (cdr set)))))
  
(define (encode-symbol symbol tree)

  ;; Version of reverse that runs in O(n) linear time
  (define (reverse items)
    (define (reverse-iter lst1 lst2)
      (if (null? lst1)
	  lst2
	  (reverse-iter (cdr lst1) (cons (car lst1) lst2))))
    (reverse-iter items '()))

  ;; Build up the encoding using constant time "cons", 
  ;; and then reverse the list once done.. should be 
  ;; faster than repeatedly invoking "append".
  (define (encode-symbol-iter working total)
    (if (leaf? working)
	(reverse total)
	(let ((symbols-left (symbols (left-branch working)))
	      (symbols-right (symbols (right-branch working))))
	  (cond ((element-of-set? symbol symbols-left)
		 (encode-symbol-iter (left-branch working) (cons 0 total)))
		((element-of-set? symbol symbols-right)
		 (encode-symbol-iter (right-branch working) (cons 1 total)))
		(else 
		 (error "Bad symbol: ENCODE-SYMBOL" symbol))))))
  (encode-symbol-iter tree '()))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;;
;; Let's test it out using the same sample tree as before:
;;
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

;;
;; Let's run some unit tests:
;;

(left-branch sample-tree)
;; ==> (leaf a 4)

(left-branch (right-branch sample-tree))
;; ==> (leaf b 2)

;;
;; Inspecting the sample tree, we deduce that the following encodings should hold:
;;
;;  a <== 0
;;  b <== 10 
;;  c <== 111
;;  d <== 110
;;

;;
;; Let's see if those are the encodings that we get from the procedure:
;;
(encode-symbol 'a sample-tree)
;; ==> (0)
(encode-symbol 'b sample-tree)
;; ==> (1 0)
(encode-symbol 'c sample-tree)
;; ==> (1 1 1)
(encode-symbol 'd sample-tree)
;; ==> (1 1 0)

;;
;; Let's try to encode a symbol not in the tree:
;;
(encode-symbol 'e sample-tree)
;; ==> Bad symbol: ENCODE-SYMBOL e

;;
;; Looks good so far..
;;
;; Let's see if we can encode the message that was given in the previous exercise:
;;
(encode '(a d a b b c) sample-tree)
;; ==> (0 1 1 0 0 1 0 1 0 1 1 1)

(decode (encode '(a d a b b c) sample-tree) sample-tree)
;; ==> (a d a b b c)