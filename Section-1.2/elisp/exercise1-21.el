;;
;; Exercise 1.21
;;
;; Use the smallest-divisor procedure to find the smallest divisor of each 
;; of the following numbers: 199, 1999, 19999
;;

;;
;; Define the procedures
;;
(defun smallest-divisor (n)
  (defun square (n) (* n n))
  (defun divides? (a b) (= (% b a) 0))
  (defun find-divisor (test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (t (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

;;
;; Run the tests
;;
(smallest-divisor 199)
;; --> 199

(smallest-divisor 1999)
;; --> 1999 

(smallest-divisor 19999)
;; --> 7