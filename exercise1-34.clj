;;
;; Exercise 1.34
;;
;; Suppose we define the procedure:
;;
;; (define (f g) (g 2))
;;
;; Then we have:
;;
;; (f square)
;; 4
;;
;; (f (lambda (z) (* z (+ z 1))))
;; 6
;;
;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
;;

;;
;; Define the procedure f and supporting procedures:
;;
(defn f [g] (g 2))

(defn square [n] (* n n))

;;
;; Expand the first example:
;;
(f square)
(square 2)
(* 2 2)
4


;;
;; Expand the second example:
;;
(f (fn [z] (* z (+ z 1))))
((fn [z] (* z (+ z 1))) 2)
(* 2 (+ 2 1))
(* 2 3)
6

;;
;; Expand the expression (f f):
;;
(f f)
(f 2)
(2 2)
;; ==> java.lang.ClassCastException: java.lang.Integer cannot be cast to clojure.lang.IFn

;;
;; 2 is not an operator or procedure, and the "argument" 2 cannot be applied to the "operator" 2 in the expression (2 2)
;;