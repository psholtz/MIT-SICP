
;; [WORKING]

;;
; Define "accumulate" and "accumulate-n":
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
;; ==>
(accumulate * 1 (list 1 2 3 4 5))
;; ==>
(accumulate cons '() (list 1 2 3 4 5))
;; ==>

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
;; Let's define "a" to the following matrix:
;;
;;  | 1  2  3 |
;;  | 4  2  1 |
;;
;; and let's define "b" to be the following matrix:
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

(matrix-*-matrix 

;;
;; Note that we could also define the matrix-*-matrix procedure as follows:
;;
;; (define (matrix-*-matrix m n)
;;  (let ((cols (transpose n)))
;;   (map (lambda (x)
;;          (map (lambda (y)
;;                 (dot-product x y)) cols)) m)))
;;