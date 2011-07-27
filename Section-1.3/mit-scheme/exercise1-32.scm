;;
;; Exercise 1.32
;;
;; (a) Show that "sum" and "product" (exercise 1.31) are both special cases of a still more general
;; notion called "accumulate" that combines a collection of terms, using some general accumulation 
;; function:
;;
;; (accumulate combiner null-value term a next b)
;;
;; "accumulate" takes as arguments the same term and range specifications as "sum" and "product", 
;; together with a "combiner" procedure (of two arguments) that specifies how the current term 
;; is to be combined with the accumulation of the preceding terms and a "null-value" that specifies
;; what base value to use when the terms run out. Write "accumulate" and show how "sum" and "product"
;; can both be defined as simple calls to "accumulate".
;;

;; 
;; First, define the "accumulate" procedure:
;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; 
;; Define some supporting procedures:
;;
(define (identity n) n)
(define (inc n) (+ n 1))
(define (cube n) (* n n n))

;;
;; Define some of the procedures in the text as calls to accumluate:
;;
(define (sum-integers a b)
  (accumulate + 0 identity a inc b))
(define (sum-cubes a b)
  (accumulate + 0 cube a inc b))
(define (factorial n)
  (accumulate * 1 identity 1 inc n))

;;
;; We can redefine "sum" in terms of "accumulate" as follows:
;;
(define (sum term a next b)
  (accumulate + 0 term a next b))

;;
;; We can redefine "product" in terms of "accumulate" as follows:
;;
(define (product term a next b)
  (accumulate * 1 term a next b))

;;
;; With these procedures, we can redefine procedures above in terms of the new "sum" and "product":
;;
(define (sum-integers a b)
  (sum identity a inc b))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (factorial n)
  (product identity 1 inc n))

;;
;; (b) If your accumulate procedure generates a recursive process, write one that generates an iterative
;;     process. If it generates an iterative process, write one that generates a recursive process.
;;

;; 
;; The definition given above generates a recursive process.
;;
;; An example of an "accumulate" procedure that generates an iterative process is given below:
;;
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
	result
	(accumulate-iter (next a) (combiner (term a) result))))
  (accumulate-iter a null-value))