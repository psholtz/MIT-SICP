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
(define (f g) (g 2))

(define (square x) (* x x))

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
(f (lambda (z) (* z (+ z 1))))
((lambda (z) (* z (+ z 1))) 2)
(* 2 (+ 2 1))
(* 2 3)
6

;; 
;; Expand the expression (f f):
;;
(f f)
(f 2)
(2 2)
;; 
;; ERROR -> the object 2 is not applicable.
;;

;;
;; 2 is not an operator or procedure, and the "argument" 2 cannot be applied to the "operator" 2 in the expression (2 2)
;;
