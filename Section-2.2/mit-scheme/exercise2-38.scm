;;
;; Exercise 2.38
;;

;;
;; The "fold-left" procedure as defined in the exercise:
;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;;
;; The "accumulate" procedure renamed as "fold-right":
;;
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;
;; Answers to the questions given:
;;
(fold-right / 1 (list 1 2 3))
;; ==> 3/2
(fold-left / 1 (list 1 2 3))
;; ==> 1/6

(fold-right list '() (list 1 2 3))
;; ==> (1 (2 (3 ())))
(fold-left list '() (list 1 2 3))
;; ==> (((() 1) 2) 3)

;;
;; Let's expand why these answers are different by looking at the respective call graphs.
;; We'll start with the "fold-right" procedure for the first problem:
;;
(fold-right / 1 (list 1 2 3))
(/ 1 (fold-right / 1 (list 2 3)))
(/ 1 (/ 2 (fold-right / 1 (list 3))))
(/ 1 (/ 2 (/ 3 (fold-right / 1 '()))))
(/ 1 (/ 2 (/ 3 1)))
;; ==> 3/2

;; 
;; and now for "fold-left":
;;
(fold-left / 1 (list 1 2 3))
(iter 1 (list 1 2 3))
(iter (/ 1 1) (list 2 3))
(iter 1 (list 2 3))
(iter (/ 1 2) (list 3))
(iter (/ (/ 1 2) 3) '())
(/ (/ 1 2) 3)
;; ==> 1/6

;;
;; We see that "fold-right" proceeds from the "right" and applies the operation. In other words, folding (list 1 2 3)
;; from the "right" is tantamount to calculating "3 divided by 2 divided by 1", which gives 3/2. On the other hand, 
;; folding from the "left" is tantamount to applying the operator to the list in succession from the left side. So that
;; folding (list 1 2 3) from the left would be tantamount to calculating "1 divided by 2 divided by 3", or 1/6.
;; 

;;
;; Let's step through the same calculation, but for the second problem:
;;
(fold-right list '() (list 1 2 3))
(list 1 (fold-right list '() (list 2 3)))
(list 1 (list 2 (fold-right list '() (list 3))))
(list 1 (list 2 (list 3 (fold-right list '() '()))))
(list 1 (list 2 (list 3 '())))
;; ==> (1 (2 (3 '())))

;; 
;; And again from the "left" direction:
;;
(fold-left list '() (list 1 2 3))
(iter (list '() 1) (list 2 3))
(iter (list (list '() 1) 2) (list 3))
(iter (list (list (list '() 1) 2) 3) '())
(list (list (list '() 1) 2) 3)
;; ==> (((() 1) 2) 3)

;;
;; To find the property that "op" must satisfy for "fold-right" and "fold-left" to be equal, consider folding a 
;; simple two-element list (i.e., (list x y)). Based on the preceding discussion, we must have "x op y" equal 
;; to "y op x". In other words, "op" must be commutative.
;;
(fold-right + 0 (list 1 2 3))
;; ==> 6
(fold-left + 0 (list 1 2 3))
;; ==> 6