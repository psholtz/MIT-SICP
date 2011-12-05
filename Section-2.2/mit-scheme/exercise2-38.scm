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

;; What is the property?