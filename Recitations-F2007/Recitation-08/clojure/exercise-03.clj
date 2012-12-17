;;
;; Working definitions
;;
(defn variable? [exp]
  (symbol? exp))
(defn make-variable [var]
  var)
(defn variable-name [exp]
  exp)

(defn or? [exp]
  (and (list? exp) (= (first exp) 'or)))
(defn make-or [exp1 exp2]
  (list 'or exp1 exp2))
(defn or-first [exp]
  (first (rest exp)))
(defn or-second [exp]
  (first (rest (rest exp))))

(defn and? [exp]
  (and (list? exp) (= (first exp) 'and)))
(defn make-and [exp1 exp2]
  (list 'and exp1 exp2))
(defn and-first [exp]
  (first (rest exp)))
(defn and-second [exp]
  (first (rest (rest exp))))

;;
;; Previous exercises
;;
(defn not? [exp]
  (and (list? exp) (= (first exp) 'not)))
(defn make-not [exp]
  (list 'not exp))
(defn not-first [exp]
  (first (rest exp)))

;;
;; Exercise 3
;;
;; Given a boolean expression and a set of variable assignments, evaluate the expression to
;; decide whether the result if #t or #f. Assume that you have a procedure (variable-value
;; name environment), which takes a variable and and a list of values and returns the value
;; assigned to the variable, if a binding for it exists, or throws an error if no binding is
;; found.
;;

;;
;; As with some of the other examples in Chapter 2, it's easier to get a handle on these
;; exercies if we peek ahead to Chapter 3 and use the "table" structure defined there, to
;; use for our symbol bindings and execution environment.
;;
;; We'll import the relevant table definitions:
;;
(defn assoc [key records]
  (cond ((empty? records) false)
	((= key (first (first records))) (first records))
	:else
	(assoc key (rest records))))

(defn insert! [key value table]
  (let [record (assoc key (rest table))]
    (if record
      
(defn make-table [] (list '*table*))

;;
;; Let's define a symbol table to use as our environment:
;;
(def env (make-table))