;;
;; Exercise 8
;;
;; Write a procedure that will multiply two positive integers together, but the 
;; only arithmetic operation allowed is addition (i.e., multiplication through
;; repeated addition). In addition, your procedure should be iterative, not 
;; recursive. What is its order of growth?
;;

;;
;; For simplicity, we will only model numbers x,y where x and y are non-negative integers.
;;
(define (slow-mul x y)
  (define (iter z total)
    (if (= z y)
	total
	(iter (+ z 1) (+ total x))))
  (iter 0 0))

;;
;; Run some unit tests:
;;
(slow-mul 0 0)
;; ==> 0
(slow-mul 1 0)
;; ==> 0
(slow-mul 5 0)
;; ==> 0
(slow-mul 0 5)
;; ==> 0
(slow-mul 0 1)
;; ==> 0
(slow-mul 1 1)
;; ==> 1 
(slow-mul 2 3)
;; ==> 6
(slow-mul 3 2)
;; ==> 6

;; [working --> orders of growth]