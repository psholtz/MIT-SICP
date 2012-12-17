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
;; ==> ERROR: invalid function (quote +)

;; (f) (if '(= x 0) 7 8)
(if '(= x 0) 7 8)
;; ==> 7
 
;; (g) (eq 'x 'X)
(eq 'x 'X)
;; ==> nil

;; (h) (eq (list 1 2) (list 1 2))
(eq (list 1 2) (list 1 2))
;; ==> nil

;; (i) (equal? (list 1 2) (list 1 2))
(equal? (list 1 2) (list 1 2))
;; ==> t 

;; (j) (let ((a (list 1 2))) (eq a a))
(let ((a (list 1 2))) (eq a a))
;; ==> t
