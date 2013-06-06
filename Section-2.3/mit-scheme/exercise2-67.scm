;;
;; Exercise 2.67
;;
;; Define an encoding tree and a sample message:
;;
;; (define sample-tree
;;  (make-code-tree (make-leaf 'A 4)
;;                  (make-code-tree
;;                   (make-leaf 'B 2)
;;                    (make-code-tree (make-leaf 'D 1)
;;                                    (make-leaf 'C 1)))))
;;
;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;
;; Use the "decode" procedure to decode the message and give the result.
;;

;;
;; First let's import the relevant Huffman tree procedures.
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

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else
	  (error "Bad bit -- CHOOSE BRANCH" bit))))

;;
;; Now, let's define the symbol tree and the sample message, as specified 
;; in the problem statement:
;;
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;; ==> (a d a b b c a)