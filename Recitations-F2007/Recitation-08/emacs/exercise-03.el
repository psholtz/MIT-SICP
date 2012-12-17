
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
