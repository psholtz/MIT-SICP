;;
;; Exercise 2.39
;;
;; Complete the following definitions of "reverse" (exercise 2.16) in terms of "fold-right" and "fold-left" from
;; exercise 2.38:
;; 
;; (define (reverse sequence) 
;;  (fold-right (lambda (x y) <??>) '() sequence))
;; (define (reverse sequence)
;;  (fold-left (lambda (x y) <??>) '() sequence))
;;

;;
;; Define the "fold-right" procedure:
;;
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;
;; Define the "fold-left" procedure:
;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;;
;; Let's think about the properties the mystery *op* must satisfy:
;;
;; (fold-right *op* '() (list 1 2 3))
;; (*op* 1 (fold-right *op* '() (list 2 3)))
;; (*op* 1 (*op* 2 (fold-right *op* '() (list 3))))
;; (*op* 1 (*op* 2 (*op* 3 (fold-right *op* '() '()))))
;; (*op* 1 (*op* 2 (*op* 3 '())))
;;
;; Tinkering around, we see that the "lambda" which will satisfy the constraints we have is given by:
;;
;; (lambda (x y) (if (null? y) (list x) (append y (list x))))
;;

;;
;; Hence, the definition "reverse" in terms of "fold-right":
;;
(define (reverse sequence)
  (fold-right (lambda (x y)
		(list x)
		(append y (list x))) '() sequence))

;;
;; Run unit test:
;;
(reverse (list 1 2 3))
;; ==> (3 2 1)

;;
;; Let's think about the properties the mystery *op* must satisfy:
;;
;; (fold-left *op* '() (list 1 2 3))
;; (iter (*op* '() 1) (list 2 3))
;; (iter (*op* (*op* '() 1) 2) (list 3))
;; (iter (*op* (*op* (*op* '() 1) 2) 3) '())
;; (*op* (*op* (*op* '() 1) 2) 3)
;;
;; Tinkering around, we see that the "lambda" which will satisfy the constraints we have is given by:
;;
;; (lambda (x y) (if (null? x) (list y) (append (list y) x)))
;;

;;
;; Hence, the definition of "reverse" in terms of "fold-left":
;;
(define (reverse sequence)
  (fold-left (lambda (x y) 
	       (if (null? x)
		   (list y)
		   (append (list y) x))) '() sequence))

;;
;; Run unit test:
;;
(reverse (list 1 2 3))
;; ==> (3 2 1)