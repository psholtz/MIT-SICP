;;
;; Exercise 1.31
;;
;; (a) The "sum" procedure is only the simplest of a vast number of similar abstractions that can be 
;; captured as higher-order procedures. Write an analogous procedure called "product" that returns
;; the product of the values of a function at points over a given range. Show how to define factorial
;; in terms of "product". Also use "product" to compute approximations to pi using the formula:
;;
;; pi/4 = ( 2 * 4 * 4 * 6 * 6 * 8 * 8 * ... ) / ( 3 * 3 * 5 * 5 * 7 * 7 * ...)
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
(define (pi-partial n)
  ;;
  ;; The mapping from n to "numerator" that we desire is as follows:
  ;;
  ;; 1 --> 2
  ;; 2 --> 4
  ;; 3 --> 4
  ;; 4 --> 6
  ;; 5 --> 6
  ;; ...
  ;;
  (define (numerator n)
    (cond ((even? n) (+ n 2.0))
	  (else
	   (+ n 1.0))))

  ;;
  ;; The mapping from n to "denominator" that we desire is as follows:
  ;;
  ;; 1 --> 3
  ;; 2 --> 3
  ;; 3 --> 5
  ;; 4 --> 5
  ;; 5 --> 7
  ;; ...
  ;;
  (define (denominator n)
    (cond ((even? n) (+ n 1.0))
	  (else
	   (+ n 2.0))))

  ;;
  ;; The "term" will be the fraction:
  ;;
  (define (term n)
    (/ (numerator n) (denominator n)))

  ;;
  ;; Write it all in terms of "product"
  ;;
  (product term 1.0 inc n))

;;
;; The partial approximations only tend to pi/4.
;;
;; Have to multiply by 4 to get back to pi.
;;
(define (pi n)
  (* 4 (pi-partial n)))

(pi 1)
;; --> 2.666666

(pi 5)
;; --> 2.92571428

(pi 10)
;; --> 3.27510104

(pi 20)
;; --> 3.21378494

(pi 50)
;; --> 3.1719440917

(pi 100)
;; --> 3.1570301

(pi 500)
;; --> 3.1447232866

(pi 1000)
;; --> 3.1431607

(pi 5000)
;; --> 3.1419067

(pi 10000)
;; --> 3.1417497

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

;;
;; We can try testing this by re-running our factorial expressions:
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