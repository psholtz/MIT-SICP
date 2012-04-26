;;
;; Exercise 1
;;
;; Draw box-and-pointer diagrams for the values of the following expressions.
;; Also give the printed representation.
;;
;; (a) (cons 1 2)
;;
;; (b) (cons 1 (cons 3 (cons 5 '())))
;;
;; (c) (cons (cons (cons 3 2) (cons 1 0)) '())
;;
;; (d) (cons 0 (list 1 2))
;;
;; (e) (list (cons 1 2) (list 4 5) 3)
;;

;;
;; Clojure does not have the same notion of CONS as do other versions of Lisp.
;;
;; Unlike traditional versions of Lisp, in Clojure lists are not the primary data
;; structure. Data structures in Clojure can implement the ISeq interface, and
;; the "seq?" procedure checks whether an object implements ISeq. When "cons" is
;; used in Clojure, the [...]