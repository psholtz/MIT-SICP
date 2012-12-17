;;
;; Exercise 1
;;
;; Evaluation -- give the printed value, assuming x is bound to 5.
;;

;; (a) '3
;; ==> 3

;; (b) 'x
;; ==> x

;; (c) ''x
;; ==> (quote x)

;; (d) (quote (3 4))
;; ==> (3 4)

;; (e) ('+ 3 4)
;; ==> 4
;; [WORKING] ==> EXPLAIN THIS!

;; (f) (if '(= x 0) 7 8)
;; ==> 7

;; (g) (eq? 'x 'X)
;;
;; In clojure we must rewrite this as: (= 'x 'X)
;;
;; Clojure has no in-built notion of the difference between
;; "eq" and "equal" as some other Lisps do. In clojure, =
;; works on symbols and evaluates true for lists with
;; identical members.
;;
(= 'x 'X)
;; ==> false 

;; (h) (eq? (list 1 2) (list 1 2))
;;
;; Again, rewriting:
;;
(= (list 1 2) (list 1 2))
;; ==> true

;; (i) (equal? (list 1 2) (list 1 2))
;;
;; Again, rewriting:
;;
(= (list 1 2) (list 1 2))
;; ==> true