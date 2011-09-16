;;
;; Exercise 1.5
;;
;; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using
;; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
;;
;; (define (p) (p))
;;
;; (define (test x y)
;;  (if (= x 0)
;;       0
;;       y))
;;
;; Then he evaluates the expression:
;;
;; (test 0 (p))
;;
;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What
;; behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
;; (Assume that the evaluation rule for the special form "if" is the same whether the interpreter
;; is using normal or applicative order: the predicate expression is evaluated first, and the result
;; determines whether to evaluate the consequent or the alternative expression).
;;

;;
;; Define Ben Bitdiddle's two procedures:
;;
(defn p [] (p))

(defn test [x y]
  (if (= x 0)
    0
    y))

;;
;; Run the test procedure.
;;
(test 0 (p))

;;
;; On a standard Scheme interpreter, which uses applicative-order evaluation,
;; this will result in an infinite recursion, hanging the interpreter.
;; On the other hand, if the interpreter uses normal-order evaluation,
;; the procedure will return 0.
;;