;;
;; Exercise 2.37
;;
;; Suppose we represent vectors v = v(i) as sequences of numbers, and matrices m = m(ij) as sequences 
;; of vectors (the rows of the matrix). For example, the matrix:
;;
;; | 1  2  3  4 |
;; | 4  5  6  6 |
;; | 6  7  8  9 |
;;
;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation we can use
;; any sequence operations to concisely express the basic matrix and vector operations. These operations
;; (which are described in any book on matrix algebra) are the following:
;;
;; (dot-product u v) 
;; (matrix-*-vector m v)
;; (matrix-*-matrix m n)
;; (transpose m)
;;
;; We can define the dot product as:
;;
;; (define (dot-product v w)
;;  (accumulate + 0 (map * v w)))
;;
;; Fill in the missing expressions in the following procedures for computing the other matrix operations.
;; (The procedure "accumulate-n" is defined in exercise 2.36).
;;
;; (define (matrix-*-vector m v)
;;  (map <??> m))
;;
;; (define (transpose m)
;;  (accumulate-n <??> <??> m))
;;
;; (define (matrix-*-matrix m n)
;;  (let ((cols (transpose n)))
;;   (map <??> m)))
;;

;;
; Define "accumulate" and "accumulate-n":
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
;; ==> 15
(accumulate * 1 (list 1 2 3 4 5))
;; ==> 120
(accumulate cons '() (list 1 2 3 4 5))
;; ==> (1 2 3 4 5)

(define (accumulate-n op init args)
  (if (null? (car args))
      '()
      (cons (accumulate op init (map car args))
	    (accumulate-n op init (map cdr args)))))

(define s (list 
	   (list 1 2 3)
	   (list 4 5 6)
	   (list 7 8 9)
	   (list 10 11 12)))

(accumulate-n + 0 s)
;; ==> (22 26 30)
	     
;;
;; Definition given in the textbook:
;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;
;; Define the support matrix operations:
;;
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
	   (matrix-*-vector cols x)) m)))

;;
;; Run some unit tests:
;;

;;
;; Let's define "a" as:
;;
;;  | 1  2  3 |
;;  | 4  2  1 |
;;
;; and let's define "b" as:
;;
;;  | 3  4 |
;;  | 1  2 |
;;  | 9  1 |
;;
;; These two matrices can be multiplied together. 
;;
;; Let's run these matrices through the procedures we've defined.
;;
(define a (list (list 1 2 3) (list 4 2 1)))
(define b (list (list 3 4) (list 1 2) (list 9 1)))

(matrix-*-matrix a b)
;; ==> ((32 11) (23 21))

;;
;; Note that we could also define the matrix-*-matrix procedure as follows:
;;
;; (define (matrix-*-matrix m n)
;;  (let ((cols (transpose n)))
;;   (map (lambda (x)
;;          (map (lambda (y)
;;                 (dot-product x y)) cols)) m)))
;;