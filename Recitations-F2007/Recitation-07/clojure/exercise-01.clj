;;
;; Exercise 1
;;
;; Write a function "occurrences" that takes a number and a list and counts the number
;; of times the number appears in the list. Write two versions -- one that uses "filter"
;; and one that uses "fold-right". For example:
;;
;; (occurrences 1 (list 1 2 1 1 3)) ==> 3
;;

;;
;; First let's define "occurrences" in a "naive" way, using simple recursion:
;;
(ns sicp.clojure.lisp)

(defn occurrences [num a]
  (if (empty? a)
    0
    (if (= num (first a))
      (+ 1 (occurrences num (rest a)))
      (occurrences num (rest a)))))

;;
;; Unit tests:
;;
(occurrences 1 (list 1 2 1 1 3))
;; ==> 3

(occurrences 2 (list 1 2 1 1 3))
;; ==> 1

(occurrences 3 (list 1 2 1 1 3))
;; ==> 1

(occurrences 4 (list 1 2 1 1 3))
;; ==> 0

;;
;; Now write the definition using the "filter" procedure:
;;
(defn occurrences [num a]
  (defn predicate [x]
    (if (= x num)
      true
      false))
  (count (filter predicate a)))

;;
;; Running the unit tests defined above gives the same results.
;;

;;
;; Now write the definition using the "fold-right" procedure:
;;
(defn fold-right [op init lst]
  (if (empty? lst)
    init
    (op (first lst)
	(fold-right op init (rest lst)))))

(defn occurrences [num a]
  (defn func [x y]
    (if (= x num)
      (concat y (list x))
      y))
  (count (fold-right func '() a)))

;;
;; Running the unit tests defined above gives the same results.
;;

