;;
;; Exercise 1.5
;;

;;
;; Define Ben Bitdiddle's two procedures:
;;
(defun p () (p))

(defun test (x y)
  (if (= x 0) 0 y))

;;
;; Run the test procedure.
;;
(test 0 (p))

;; 
;; On a standard Lisp interpreter, which uses applicative-order evaluation, 
;; this will result in an infinite recursion, hanging the interpreter. 
;; On the other hand, if the interpreter uses normal-order evaluation,
;; the procedure will return 0.
;;

