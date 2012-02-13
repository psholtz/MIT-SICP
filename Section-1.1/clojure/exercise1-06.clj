;;
;; Execise 1.6
;;
;; Alyssa P. Hacker doesn't see why "if" needs to be provded as a special form. "Why can't I just
;; define it as an ordinary procedure in terms of "cond"?" she asks. Alyssa's friend Eva Lu Ator
;; claims this can indeed be done, and she defines a new version of "if":
;;
;; (define (new-if predicate then-clause else-clause)
;;  (cond (predicate then-clause)
;;        (else else-clause)))
;;
;; Eva demonstrates the program for Alyssa:
;;
;; (new-if (= 2 3) 0 5)
;; ==> 5
;;
;; (new-if (= 1 1) 0 5)
;; ==> 0
;;
;; Delighted, Alyssa uses "new-if" to rewrite the square-root program:
;;
;; (define (sqrt-iter guess x)
;;  (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x)
;;                       x)))
;;
;; What happens when Alyssa attempts to use this to compute square roots? Explain.
;;

;;
;; Define the new "if" predicate as follows:
;;
(defn new-if
  {:doc "New 'if' predicate, *not* defined as a special form"}
  [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(new-if (= 2 3) 0 5)
;; ==> 5

(new-if (= 1 1) 0 5)
;; ==> 0

;;
;; Rewrite the "sqrt" procedure using the new "if" predicate:
;;
(defn square
  {:doc "Return the square of the argument"}
  [n] (* n n))

(defn good-enough?
  {:doc "Is the square of guess close enough to x?"}
  [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn average
  {:doc "Return the average of x and y"}
  [x y]
  (/ (+ x y) 2.0))

(defn improve
  {:doc "Improve the 'guess', so that its square will be closer to 'x'"}
  [guess x]
  (average guess (/ x guess)))

;;
;; INSERT NEW "IF" PREDICATE HERE
;;
(defn sqrt-iter
  {:doc "Recursive procedure used to generate approximations to square root x"}
  [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  {:doc "Wrapper function, used to invoke the square root procedure"}
  (sqrt-iter 1.0 x))

;;
;; Attempting to run this code through the Scheme interpreter, which
;; uses applicative-order evaluation, will result in an infinite
;; recursion. Because new-if is a procedure, rather than a special form,
;; the interpreter will attempt to evaluate the procedure and its
;; operands, before applying the procedure to the operands.
;; In this case, the way in which sqrt-iter is recursively defined, will
;; cause the interpreter to go into an infinite recursion.
;;
;; Roughly speaking, the evaluation proceeds as follows:
;;
;; (sqrt-iter 1.0 2.0)
;; (new-if (good-enough? 1.0 2.0) 1.0 (sqrt-iter (improve 1.0) 2.0))
;;
;; At this point, the interpreter will attempt to evaluate the operands
;; to new-if, before applying new-if to the operands. (good-enough? 1.0 2.0)
;; evaluates to #f, but sqrt-iter is recursively defined, and includes in
;; its definition another instance of new-if:
;;
;; (sqrt-iter (improve 1.0) 2.0)
;; (new-if (good-enough? (improve 1.0) 2.0) (improve 1.0) (sqrt-iter (improve (improve 1.0)) 2.0))
;;
;; It's easy to see that this process results in an infinite recursion, hanging
;; the interpreter.
;;

;;
;; This code will hang the interpreter:
;;
(sqrt-iter 1.0 2.0)