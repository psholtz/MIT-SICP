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
;; Let's see if we can rewrite it using the "fold-right" procedure:
;;
(fold-right 
 (lambda (a b) (list (list a) b)) 
 '()
 (filter (lambda (y) (even? y)) x))

;; ==> ((2) ((4) ((6) ())))     

;;
;; Which again, is the answer we are seeking.
;;

;;
;; The reasons this work is that we first extract the "even" numbers using the filter procedure:
;;
(filter (lambda (y) (even? y)) x)
;; ==> (2 4 6)

;;
;; And then we "fold from the right" using the custom-defined anonymous function 
;; (lambda (a b) (list (list a b) b)) to generate the final result.
;;

;;
;; (e) The maximum element of x: 7
;;

;;
;; Let's walk down the list structure using "fold-right" and compare each two successive
;; elements, saving the largest one. We need to "initialize" the algorithm using a number
;; we know will be "smaller" than the smallest element in the list. In C/C++ we might set
;; this variable to something like (-1) * MAX_INT or some such value. 
;;
;; Here, let's just set this sentinel value to something like -1000, since we know that all 
;; the elements in the list will be larger than this value:
;;
(fold-right (lambda (a b) (if (> a b) a b))
	    -1000
	    x)

;; ==> 7

;;
;; Playing around with this procedure on other lists shows that it returns the maximum 
;; element (even if that element is not the last element in the last structure), so long 
;; as the "minimum sentinel" value is correctly initialized.
;;

;;
;; (f) A list of the last element of x: (7)
;;

;; [working]

;;
;; Another way this can be done is to use the "fold-left" procedure:
;;
(fold-left 
 (lambda (a b)
   (if (null? a)
       (list b)
       '()))
 '()
 x)

;; ==> (7)

;;
;; As an aside, it is note that we can obtain the "first" element in the list using a procedure 
;; like this:
;;
(fold-right 
 (lambda (a b)
   (if (null? b)
       (list a)
       '()))
 '()
 x)

;; ==> (1)

;;
;; The utility of this approach is dubious, given the existence of the "car" procedure, but is
;; cited here nevertheless.
;;