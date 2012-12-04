;;
;; Exercise 1 
;;
;; Evaluation -- give the printed value, assuming x is bound to 5.
;;

;; (a) '3
'3
;; ==> 3

;; (b) 'x
'x
;; ==> x

;; (c) ''x
''x
;; ==> (quote x)

;; (d) (quote (3 4))
(quote (3 4))
;; ==> (3 4)

;; (e) ('+ 3 4)
('+ 3 4)
;; ==> Error: the object + is not applicable

;; (f) (if '(= x 0) 7 8)
(if '(= x 0) 7 8)
;; ==> 7

;;
;; Note that here, what is being tested in the "if" predicate
;; is not whether the symbol "x" is equal to 0 (which it is not, 
;; it is bound to 5), but rather whether the symbol '(= x 0) is 
;; non-nil, which in fact it is.
;;

;; (g) (eq? 'x 'X)
(eq? 'x 'X)
;; ==> #t

;; (h) (eq? (list 1 2) (list 1 2))
(eq? (list 1 2) (list 1 2))
;; ==> #f

;;
;; Note that eq? compares whether the two arguments
;; represent the same object in memory. Since we are 
;; dealing here with two different objects, the 
;; procedure returns false.
;;

;; (i) (equal? (list 1 2) (list 1 2))
(equal? (list 1 2) (list 1 2))
;; ==> #t

;;
;; Note that equal? compares the values of the arguments, 
;; and returns true if they have congruent structures. 
;; This may have different meanings for different data 
;; structures, but clearly in this case, the two lists
;; are "equal".
;;

;; (j) (let ((a (list 1 2))) (eq? a a))
(let ((a (list 1 2))) (eq? a a))
;; ==> #t
