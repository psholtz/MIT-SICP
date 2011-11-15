;;
;; Exercise 2.20
;;
;; The procedures +, *, and "list" take arbitrary numbers of arguments. One way to define such procedures
;; is to use "define" with dotted-tail notation. In a procedure, a parameter list that has a dot before the 
;; last parameter name indicates that, when the procedure is called, the initial parameters (if any) will hae as 
;; values the initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments.
;; For instance, given the definition
;;
;; (define (f x y . z) <body>)
;;
;; the procedure "f" can be called with two or more arguments. If we evaluate
;;
;; (f 1 2 3 4 5 6)
;;
;; then in the body of "f", "x" will be 1, "y" will be 2 and "z" will be the list (3 4 5 6). Given the definition
;;
;; (define (g . w) <body>0
;;
;; the procedure "g" can be called with zero or more arguments. If we evaluate
;;
;; (g 1 2 3 4 5 6)
;;
;; then in the body of "g", "w" will be the list (1 2 3 4 5 6).
;;
;; Use this notation to write a procedure "same-parity" that takes one or more integers and returns a list 
;; of all the arguments that have the same even-odd parity as the first argument. For example,
;;
;; (same-parity 1 2 3 4 5 6 7)
;; ==> (1 3 5 7)
;;
;; (same-parity 2 3 4 5 6 7)
;; ==> (2 4 6)
;;

;;
;; Define the "same-parity" procedure:
;;
(define (same-parity x . z)
  ;; define the parity-testing procedures
  (define (even? n) (= (remainder n 2) 0))
  (define (odd? n) (not (even? n)))

  ;; define the list-filtering routine
  (define (filter func)
    (define (filter-iter list-in list-out)
      (cond ((null? list-in) list-out)
	    (else
	     (let ((a (car list-in)))
	       (if (func a)
		   (filter-iter (cdr list-in) (append list-out (list a)))
		   (filter-iter (cdr list-in) list-out))))))
    (filter-iter z (list x)))
  
  ;; invoke the "filter" procedure appropriately
  (cond ((even? x) (filter even?))
	(else (filter odd?))))

;;
;; Test the procedure:
;;
(same-parity 1 2 3 4 5 6 7)
;; ==> (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; ==> (2 4 6)