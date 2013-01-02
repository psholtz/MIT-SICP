;;
;; Exercise 1.7
;;
;; The good-enough? test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers. Also,
;; real computers, arithmetic operations are almost always performed with
;; limited precision. This makes our test inadequate for very large numbers.
;; Explain these statements, with examples showing how the test fails for small
;; and large numbers. An alternative strategy for implementing good-enough?
;; is to watch how guess changes from one iteration to the next and to stop
;; when the change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better for small
;; and large numbers?
;;

;;
;; The procedure fails for small numbers because the tolerance is too coarse.
;;
;; For instance, the procedure will return 0.03125 as an "answer" to
;; (sqrt 1e-10), since (< (abs (- (square 0.03125) 1e-10)) 0.001) returns true,
;; even though 0.03125 is clearly not a very good approximation to the
;; correct answer 1e-05.
;;
;; Similarly, the procedure fails for large numbers because the tolerance is too fine.
;;
;; For instance, within the limits of machine precision, the procedure will never
;; return from (sqrt 2e16), since the difference between the approximation arrived
;; at by the procedure and the actual answer is never less than 0.001.
;;
;; A better implementation would watch how guess changes from one iteration
;; to the next, and stops when the change is a very small fraction of guess.
;; This implementation is given in the following code:
;;

;;
;; Clojure has a range of "syntactic sugar" facilities which are not
;; available on more "standard" implementations of Lisp. Let's start by
;; giving the "standard" Lisp/Scheme implementation, and then give a
;; more Clojure-esque way of solving this problem.
;;
;; We'll demarcate the two solutions using namespaces:
;;
(ns sicp.clojure.lisp)

(defn square
  {:doc "Return square of the argument"}
  [n] (* n n))

(defn average
  {:doc "Return the average of x and y"}
  [x y] (/ (+ x y) 2.0))

(defn improve
  {:doc "Improve the guess, so that (square guess) comes closer to x"}
  [guess x]
  (average guess (/ x guess)))

;;
;; Modified version of good-enough?, based on fractional
;; changes, rather than on an absolute tolerance.
;; The fractional tolerance is set at 0.1%.
;;
(defn good-enough?
  {:doc "Is (square guess) close enough to x for our tolerance?"}
  [guess x]
  (< (Math/abs (- (/ (square guess) x) 1.0)) 0.001))

(defn sqrt-iter
  {:doc "Make an iterative improvement to the approximation for (sqrt x)"}
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt
  {:doc "Wrapper function to invoke iterative sqrt procedure"}
  [x]
  (sqrt-iter 1.0 x))

;;
;; Unit tests:
;;
(sqrt 2)
;; ==>
(sqrt 4)
;; ==>

;;
;; Note that (sqrt 4) does not evaluate "exactly" to 2, since we are truncating
;; the recursion at a specific tolerance level.
;;

;;
;; =================================================================================
;;

;;
;; Now let's give the most "Clojure-esque" way to approach this exercise:
;;
(ns sicp.clojure.joy)

(defn square [n] (* n n))
(defn average [x y] (/ (+ x y) 2.0))
(defn improve [guess x] (average guess (/ x guess)))
(defn good-enough? [guess x]
  (def tolerance 0.001)
  (< (Math/abs (- (/ (square guess) x) 1.0)) tolerance))

;;
;; So far, pretty much the same as before. But with Clojure we can use
;; loops to accomplish the same this that is done with "recursion" in standard Lisp:
;;
(defn sqrt [x]
  (loop [guess 1.0]
    (if (good-enough? guess x)
      guess
      (recur (improve guess x)))))

;;
;; Unit tests:
;;
(sqrt 2)
;; ==>
(sqrt 4)
;; ==>

;;
;; Note that (sqrt 4) does not evaluate "exactly" to 2, since we are truncating
;; the recursion at a specific tolerance level.
;; 