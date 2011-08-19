;;
;; Exercise 1.31
;;

;;
;; (a) The "sum" procedure is only the simplest of a vast number of similar abstractions that can be
;; captured as higher-order procedures. Write an analogous procedure called "product" that returns
;; the product of the values of a function at points over a given range. Show how to define factorial
;; in terms of "product". Also use "product" to compute approximations to pi.
;;

(defun product (term a next b)
  (if (> a b)
      1
    (* (funcall term a)
       (product term (funcall next a) next b))))

(defun identity (x) x)
(defun inc (n) (+ n 1))

(defun factorial (n)
  (product #'identity 1 #'inc n))

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
(defun square (x) (* x x))
(defun even? (n)
  (= (% n 2) 0))

(defun pi-partial (n)
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
  (defun numerator (n)
    (cond ((even? n) (+ n 2.0))
	  (t
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
  (defun denominator (n)
    (cond ((even? n) (+ n 1.0))
	  (t
	   (+ n 2.0))))

  ;;
  ;; The "term" will be the fraction:
  ;;
  (defun term (n)
    (/ (numerator n) (denominator n)))

  ;;
  ;; Write it all in terms of "product"
  ;;
  (product #'term 1 #'inc n))

;;
;; The partial approximations only tend to pi/4
;;
;; We have to multiply by 4 to get back to pi.
;;
(defun pi (n)
  (* 4 (pi-partial n)))

(pi 1)

(pi 5)

(pi 10)

(pi 20)

(pi 50)

(pi 100)

(pi 500)

(pi 1000)

(pi 5000)

(pi 10000)

;;
;; (b) If your "product" procedure generates a recursive process, write one that generates an iterative
;; process. If it generates an iterative process, write one that generates a recursive process.
;;

;;
;; The code we wrote above generates a recursive process.
;;
;; Below we will implement an iterative process:
;;
(defun product (term a next b)
  (defun iter (a result)
    (if (> a b)
	result
      (iter (funcall next a) (* result (funcall term a)))))
  (iter a 1))

(factorial 0)
;; ==> 1

(factorial 1)
;; ==> 1
(factorial 2)
;; ==> 2

(factorial 3)
;; ==> 6

(factorial 4)
;; ==> 24 

(factorial 5)
;; ==> 120