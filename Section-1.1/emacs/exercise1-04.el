;;
;; Exercise 1.4
;;
;; Observe that our model of evaluation allows for combinations whose operators are compound expressions. 
;; Use this observation to describe the behavior of the following procedure:
;;
;; (define (a-plus-abs-b a b)
;;  ((if (> b 0) + -) a b))
;;

;; Procedure adds absolute value of b to a.
(defun a-plus-abs-b (a b)
  ((if (> b 0) + -) a b))

;; 
;; Although it runs in Scheme, this code will crash an emacs lisp interpreter.
;; The reason is that in emacs lisp, there is a separate namespace for operators.
;; The operator position of a form must be a name in that namespace, which is 
;; either a symbol naming a function or a lambda form. All other forms result
;; in an error.
;;

;;
;; A better way to re-write the procedure in emacs lisp might be:
;;
(defun a-plus-abs-b (a b)
  (if (> b 0)
      (+ a b)
    (- a b)))

;;
;; The procedure will then execute correctly:
;;
(a-plus-abs-b 1 1)
;; returns 2

(a-plus-abs-b 1 -1)
;; returns 2

;;
;; Some other forms which, in Common Lisp, should also produce the desired result include:
;;
(defun a-plus-abs-b (a b)
  (+ a (if (> b 0) b (- b))))

(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) '+ '-) a b))