;;
;; Exercise 4
;;
;; Suppose x is bound to the list (1 2 3 4 5 6 7). Using "map", "filter" and/or "fold-right", write 
;; an expression involving "x" that returns:
;;
;; (a) (1 4 9 16 25 36 49)
;;
(define x (list 1 2 3 4 5 6 7))

(map square x)

;;
;; (b) (1 3 5 7)
;;
(filter odd? x)

;;
;; (c) ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))
;;

;;
;; One simple possibility is the following:
;;
(map (lambda (y) (list y y)) x)

;;
;; or the even more pathological:
;;
(map (lambda (x) (list x x)) x)

;;
;; (d) ((2) ((4) ((6) ())))
;;

;; 
;; First let's break down what this expression actually "is":
;;
(list (list 2) (list (list 4) (list (list 6) '())))
;; ==> ((2) ((4) ((6) ())))

;;
;; Seeing what the pattern is, we can construct a function f that generates the structure we desire:
;;
(define (f x)
  (let ((a (filter even? x)))
    (define (f-iter b)
      (if (null? b)
	  '()
	  (list (list (car b)) (f-iter (cdr b)))))
    (f-iter a)))

;;
;; Running the procedure:
;;
(f x)
;; ==> ((2) ((4) ((6) ())))

;;
;; Which is what we were looking for.
;;
;; However, this method uses a custom-defined procedure, rather than the predicates we were furnished with.
;; 
;; Let's see if we can rewrite is using the "fold-right" procedure:
;;
(fold-right (lambda (a b) (list (list a) b)) 
	    '()
	    (filter (lambda (y) (even? y)) x))

;; ==> ((2) ((4) ((6) ())))     

;;
;; Which again, is the answer we are seeking.
;;

;; [WORKING --> explain this]
	    

;; 
;; [WORKING --> but redo this using fold-right]
;;