;;
;; Exercise 3
;;
;; Define "ls" to be a list of "procedures":
;;
;; (define (square x) (* x x))
;; (define (double x) (* x 2))
;; (define (inc x) (+ x 1))
;; (define ls (list square double inc))
;;
;; Now say we want a function "apply-procs" that behaves as follows:
;;
;; (apply-procs ls 4)
;;  ==> ((square 4) (double 4) (inc 4)) = (16 8 5)
;;
;; (apply-procs ls 3)
;; ==> ((square 3) (double 3) (inc 3)) = (9 6 4)
;;
;; Write a definition for "apply-procs" using "map".
;;

;;
;; First let's define the procedures that we need:
;;
(ns sicp.clojure.lisp)

(defn square [x] (* x x))
(defn double [x] (* x 2))
(defn inc [x] (+ x 1))
(def ls (list square double inc))

;;
;; The following is a "naive" definition of "apply-procs" using simple recursion:
;;
(defn apply-procs [a v]
  (if (empty? a)
    '()
    (cons ((first a) v)
	  (apply-procs (cdr a) v))))

;;
;; Run the unit tests:
;;
(apply-procs ls 4)
;; ==> (16 8 5)

(apply-procs ls 3)
;; ==> (9 6 4)

;;
;; Now let's define "apply-procs" using the "map" procedure:
;;
(defn apply-procs [a v]
  (map (fn [x] (x v)) a))

;;
;; Running the unit test as above, we get the same results.
;; 