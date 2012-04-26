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
;; As with the previous exercise, the manner in which Clojure implements
;; "cons" prevents being able to fully answer these questions.
;;
;; (a) and (c) and be answered as follows:
;;
;; (a) ==> (list 1 2 3)
;;
;; (c) ==> (list (list 1 2) (list 3 4) (list 5 6))
;;
;; (b) would be more problematic to represent in Clojure, owing to Clojure's
;; implementation of "cons".
;;