;;
;; Exercise 2.22
;;
;; Louis Reasoner tries to rewrite the first "square-list" procedure of exercise 2.21 so that 
;; it evolves an iterative process:
;;
;; (define (square-list items)
;;  (define (iter things answer)
;;    (if (null? things
;;        answer
;;        (iter (cdr things)
;;              (cons (square (car things))
;;                    answer))))
;;  (iter items '()))
;;
;; Unfortunately, defining "square-list" in this way produces the answer list in the reverse
;; order of the one desired. Why?
;;

;;
;; First let's verify that this is the case:
;;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))

(square-list (list 1 2 3))
;; ==> (9 4 1)

;;
;; In the language of Chapter 1, the "square-list" procedure defined above is a recursive procedure
;; which generates an iterative process. 
;;
;; However, the "answer" constructed as the procedure evalutes is built up in "reverse" order from 
;; what we would like. Specifically, at each step, the "head" element submitted to "cons" is the 
;; "head" (or "car") of the list, and this is pre-pended to the result array that has already been 
;; accumulated.
;;
;; Accordingly, the list is generated in reverse order.
;;
;; We can verify this by looking at a call graph for a simple invocation:
;;
(square-list (list 1 2 3))
(iter (list 1 2 3) '())
(iter (list 2 3) (cons (square 1) '()))
(iter (list 2 3) (cons 1 '()))
(iter (list 2 3) (list 1))
(iter (list 3) (cons (square 2) (list 1)))
(iter (list 3) (cons 4 (list 1)))
(iter (list 3) (list 4 1))
(iter '() (cons (square 3) (list 4 1)))
(iter '() (cons 9 (list 4 1)))
(iter '() (list 9 4 1))
;; ==> '(9 4 1)

;;
;; Louis then tries to fix his bug by interchanging the arguments to "cons":
;;
;; (define (square-list items)
;;  (define (iter things answer)
;;   (if (null? things)
;;       answer
;;       (iter (cdr things)
;;             (cons answer
;;                   (square (car things))))))
;;   (iter items nil))
;;
;; This doesn't work either. Explain.
;;

;; 
;; This time, the results obtained by Louis are even more bizzare:
;;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3))
;; ==> (((). 1) . 4) . 9)

;;
;; Let's investigate what is happening by looking at the call graph:
;;
(square-list (list 1 2 3))
(iter (list 1 2 3) '())
(iter (list 2 3) (cons '() (square 1)))
(iter (list 2 3) (cons '() 1))
(iter (list 2 3) (() . 1))
(iter (list 3) (cons (() . 1) (square 2)))
(iter (list 3) (cons (() . 1) 4))
(iter (list 3) ((() . 1) . 4))
(iter '() (cons ((() . 1) . 4) (square 3)))
(iter '() (cons ((() . 1) . 4) 9))
(iter '() (((() . 1) . 4) . 9))
;; ==>  (((() . 1) . 4) . 9)

;;
;; The problem is that "cons" expects to take an "object" as its first parameter, and a 
;; "list" (or "nil") structure as its second parameter. Here these two parameters are reversed:
;; the "list" structure is being passed in as a first argument, and the "object" we wish to 
;; have squared is being passed in as the second argument. 
;;
;; The result is a very odd-looking data structure indeed.
;;

;; 
;; A "good" iterative version of this procedure would be something that employs the "append"
;; procedure, something that might look something like:
;;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		      (list (square (car things)))))))
  (iter items '()))

(square-list (list 1 2 3))
;; ==> (1 4 9)