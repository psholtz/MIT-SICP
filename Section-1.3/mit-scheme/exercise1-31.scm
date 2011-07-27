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
;; Define some supporting procedures for calculating factorials:
;;
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (even? n)))

;;
;; Count factorials, but go by twos, e.g.,:
;;
;; 4!! = 4 * 2 = 8
;; 5!! = 5 * 3 = 15
;; 6!! = 6 * 4 * 2 = 48
;; 7!! = 7 * 5 * 3 = 105
;;
(define (factorial-by-two n)
  (cond ((= n 0) 1)
	((= n 1) 1)
	(else
	 (* n (factorial-by-two (- n 2))))))

;; 
;; Run some unit tests, to make sure we're OK
;;
(= (factorial-by-two 4) 8)
(= (factorial-by-two 5) 15)
(= (factorial-by-two 6) 48)
(= (factorial-by-two 7) 105)

;;
;; Use the "product" procedure to compute approximations to pi:
;; ATTEMPT ... might not work so well?
;;
(define (pi n)

  ;; 
  ;; Define the numerator in the expression for pi.
  ;;
  ;; Make sure the argument supplied is even.
  ;; Signal error condition by 0.0
  ;;
  (define (numerator k)
    (if (even? k)
	(/ (square (factorial-by-two k)) 2.0)
	0.0))

  ;; 
  ;; Define the denominator in the expression for pi.
  ;;
  ;; Make sure the argument supplied is odd.
  ;; Signal error condition by 0.0
  ;;
  (define (denominator k)
    (if (odd? k)
	(square (factorial-by-two k))
	0.0))

  ;;
  ;; Calculate the partial produts of pi, up to n
  ;;
  (define (pi-partial)
    (let ((d1 (* 2 n))
	  (d2 (+ (* 2 n) 1)))
      (/ (numerator d1) (denominator d2))))

  ;;
  ;; (pi-partial) will calculate an approximation for pi/2.
  ;;
  ;; We have to multiply it by 2 to get an approximation for pi.
  ;;
  (* 2. (pi-partial)))


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