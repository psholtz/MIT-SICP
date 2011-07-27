;;
;; Exercise 1.31
;;
;; (a) The "sum" procedure is only the simplest of a vast number of similar abstractions that can be 
;; captured as higher-order procedures. Write an analogous procedure called "product" that returns
;; the product of the values of a function at points over a given range. Show how to define factorial
;; in terms of "product". Also use "product" to compute approximations to pi using the formula:
;;
;; pi/4 = ( 2 * 4 * 4 * 6 * 6 * 8 * 8 ) / ( 3 * 3 * 5 * 5 * 7 *7 )
;;

;;
;; Define "product" procedure:
;;
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (factorial x)
  (product identity 1 inc x))

;;
;; Run some unit tests
;; 
(factorial 0)
;; --> 1

(factorial 1)
;; --> 1

(factorial 2)
;; --> 2

(factorial 3)
;; --> 6

(factorial 4)
;; --> 24

(factorial 5)
;; --> 120

;;
;; Use the "product" procedure to compute approximations to pi:
;;
(define (square x) (* x x))

(define (pi n)
  (define (next-pi x) (+ x 2))
  (/ (* 8. (product square 4. next-pi (+ 4. (* 2. (- n 1.)))) (+ 4. (* 2. n)))
     (product square 3. next-pi (+ 3. (* 2. n)))))

;; WORK IN PROGRESS
(define (pi n)
  (define (numer a)
    (cond ((= a 1) 2)
	  (else
	   3)))
  (define (denom a)
    (let ((t (+ (* 2 (/ (- a 1) 2)) 3)))
      (if (even? t)
	  (- t 1)
	  t)))
  (define (term a)
    (/ (numer a) (denom a)))
  (define (next a)
    (+ a 1))
  (define (pi-partial)
    (product term 1 next n))
  (* 4 (pi-partial)))


;;
;; Run some unit tests
;;

;; -->??

;;
;; (b) If your "product" procedure generates a recursive process, write one that generates an interative
;; process. If it generates an iterative process, write one that generates a recursive process.
;;

;;
;; The code we wrote above generates a recursive process. 
;;
;; Below we will implement an iterative process:
;;
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))
