;;
;; Exercise 2.57
;;
;; [WORKING]
;; 

;;
;; Let's import all the code we need to do symbolic differentiation, 
;; updating it as needed to support the new requirements:
;;

;;
;; Procedures for object identification:
;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;
;; Procedures for handling sums:
;;
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else
	 (list '+ a1 a2))))

;;
;; Procedures for handling products:
;;
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else
	 (list '* m1 m2))))

;;
;; Procedures for handling differences:
;;
(define (difference? x)
  (and (pair? x) (eq? (car x) '-)))
(define (minuend p) (cadr p))
(define (subtrahend p) (caddr p))
(define (make-difference s1 s2)
  (cond ((=number? s2 0) s1)
	((=number? s1 0) (make-product -1 s2))
	((and (number? s1) (number? s2)) (- s1 s2))
	(else
	 (list '- s1 s2))))

;;
;; Procedures for handling exponentiation:
;;
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**) (variable? (cadr x))))
(define (base p) (cadr p))
(define (exponent p) (caddr p))
(define (make-exponentiation base exp-value)
  (cond ((and (number? base) (number? exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(else
	 (list '** base exp-value))))

;;
;; Define the "deriv" procedure:
;;
(define (deriv expression var)

  (cond 
   ((number? expression) 0) 
   
   ((variable? expression)
    (if (same-variable? expression var) 1 0)) 
   
   ((sum? expression)
    (make-sum (deriv (addend expression) var)
	      (deriv (augend expression) var)))
   
   ((difference? expression)
    (make-difference (deriv (minuend expression) var)
		     (deriv (subtrahend expression) var)))
   
   ((product? expression)
    (make-sum
     (make-product (multiplier expression)
		   (deriv (multiplicand expression) var))
     (make-product (deriv (multiplier expression) var)
		   (multiplicand expression))))
   
   ((exponentiation? expression)
    (make-product
     (exponent expression)
     (make-exponentiation
      (base expression)
      (make-difference (exponent expression) 1))))
   
   (else
    (error "Unknown expression type -- DERIV" expression))))

;;
;; WORKING
;;

;;
;; We will first look at modifying the "augend" procedure. 
;;
;; In its present implementation, "augend" returns a single number (or symbol), 
;; that being, the "second" of the two summands in the addition operation:
;;
(define (augend s) (caddr s))

(augend '(+ 1 2))
;; ==> 2
(augend '(+ a b))
;; ==> b

;;
;; This was fine for the previous implementation, but in order to support
;; arbitary lists of summands, which may be of length greater than 2, this 
;; model breaks down:
;;
(define e1 '(+ 1 2))
(define e2 '(+ 1 2 3))
(define e3 '(+ 1 2 3 4))

(augend e1)
;; ==> 2
(augend e2)
;; ==> 2
(augend e3)
;; ==> 2

;;
;; Evaluation of (augend e1) still performs the way we would desire and expect, 
;; but in the case of the evaluation of expressions (augend e2) and (augend e2), 
;; we would like to generate a list of the remaining summands, something rather
;; more like (2 3) and (2 3 4), respectively. 
;; 

;;
;; In order to accomplish this, let's first define a selector which indicates 
;; whether we are dealing with a binary expression (i.e., two terms), or something
;; different:
;;
(define (binary? expression)
  (null? (cdddr expression)))

(binary? e1)
;; ==> #t
(binary? e2)
;; ==> #f
(binary? e3)
;; ==> #f

;;
;; So "binary?" works the way we want it to. 
;;
;; Let's further define selectors that extract (a) the first term in the expression;
;; (b) the second term in the expression; and (c) a list of all terms in the expression, 
;; except the first one:
;;
(define (first-term expression)
  (cadr expression))
(define (second-term expression)
  (caddr expression))
(define (all-but-first-term expression)
  (cddr expression))

(first-term e1)
;; ==> 1
(first-term e2)
;; ==> 1
(first-term e3)
;; ==> 1

(second-term e1)
;; ==> 2
(second-term e2)
;; ==> 2
(second-term e3)
;; ==> 2

(all-but-first-term e1)
;; ==> (2)
(all-but-first-term e2)
;; ==> (2 3)
(all-but-first-term e3)
;; ==> (2 3 4)

;;
;; We are ready to put it all together into a new model of "augend":
;;
(define (augend s)
  (if (binary? s)
      (second-term s)
      (cons '+ (all-but-first-term s))))

;;
;; Running unit tests:
;;
(augend e1)
;; ==> 2
(augend e2)
;; ==> (+ 2 3)
(augend e3)
;; ==> (+ 2 3 4)

;;
;; So if the sum is binary, "augend" returns the number/symbol we are seeking.
;; 
;; If the sum is the a list of arbitrary summands, greater in length than 1, 
;; "augend" returns a new summation object (list) which can be recursively 
;; invoked in succeeding operations.
;;

;;
;; We need to modify "multiplicand" in the same way:
;;
(define (multiplicand p)
  (if (binary? p)
      (second-term p)
      (cons '* (all-but-first-term p))))

;;
;; Running some unit tests:
;;
(define f1 '(* 1 2))
(define f2 '(* 1 2 3))
(define f3 '(* 1 2 3 4))

(multiplicand f1)
;; ==> 2
(multiplicand f2)
;; ==> (* 2 3)
(multiplicand f3)
;; ==> (* 2 3 4)

;;
;; Note, too, that the structure of "augend" and "multiplicand" is nearly identical.
;;
;; This suggests that we can "abstract" their commonality out into a more general procedure:
;;
(define (reduce-expression expression op)
  (if (binary? expression)
      (second-term expression)
      (cons op (all-but-first-term expression))))

(define (augend s) (reduce-expression s '+))

(define (multiplicand p) (reduce-expression p '*))


;;
;; [WORKING]
;;

;;
;; THIS IS THE ONE USE CASE WE MIGHT WANT TO TEST AGAINST DREWs:
;;


(deriv '(+ y z 5 (* 2 x t 5)) 'x)


;;
;; Use cases:
;;
(deriv 3 'x)
;; =>
(deriv 'x 'x)
;; ==>
(deriv '(+ x y) 'x)
;; ==>
(deriv '(+ x y) 'y)
;; ==>
(deriv '(+ x y) 'z)
;; ==>

;;
;; Give credit to the people who [WORKING]
;;