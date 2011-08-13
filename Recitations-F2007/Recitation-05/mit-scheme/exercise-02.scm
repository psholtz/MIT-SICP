;;
;; Exercise 2
;;
;; Write expressions whose values will print out like the following:
;;
;; (a) (1 2 3)
;;
;; (b) (1 2 . 3)
;; 
;; (c) ((1 2) (3 4) (5 6))
;;

;;
;; (a) (1 2 3)
;;
(list 1 2 3)
;; ==> (1 2 3)

;;
;; (b) (1 2 . 3)
;;
(cons 1 (cons 2 3))
;; ==> (1 2 . 3)

;;
;; (c) ((1 2) (3 4) (5 6))
;;
(list (list 1 2) (list 3 4) (list 5 6))
;; ==> ((1 2) (3 4) (5 6))