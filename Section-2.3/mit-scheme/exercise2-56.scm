;;
;; Exercise 2.56
;;
;; Show how to extend the basic differentiator to handle more kinds of expressions.
;; For instance, implement the differentiation rule:
;;
;; d/dx(u^n) = n*u^(n-1) * du/dx
;;
;; by adding a new clause to the "deriv" program and defining appropriate procedures
;; "exponentiation?", "base", "exponent" and "make-exponentiatoin". You may use the 
;; symbol ** to denote exponentiation). Build in the rules that anything raised to the 
;; power 0 is 1 and anything raised to the power 1 is itself.
;;

;;
;; First let's import the symbolic differentiation package as expressed in the text.
;;
;; We start with the selectors needed to support the "deriv" procedure.
;;
;; First the procedures that support basic symbol manipulation:
;;
(define (variable? x) (symbol? x))

(variable? 'x)
;; ==> #t
(variable? 31)
;; ==> #f

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(same-variable? 10 30)
;; ==> #f
(same-variable? 10 'x)
;; ==> #f
(same-variable? 'x 10)
;; ==> #f
(same-variable? 'x 'y)
;; ==> #f
(same-variable? 'x 'x)
;; ==> #t

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define test-value-01 0)
(define test-value-02 10)
(=number? 'x 0)
;; ==> #f
(=number? 0 0)
;; ==> #t
(=number? test-value-01 0)
;; ==> #t
(=number? test-value-01 10)
;; ==> #f
(=number? test-value-02 0)
;; ==> #f
(=number? test-value-02 10)
;; ==> #t

;;
;; Procedures for manipulating sums:
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

(make-sum 0 10)
;; ==> 10
(make-sum 0 'x)
;; ==> x
(make-sum 10 0)
;; ==> 10
(make-sum 'x 0)
;; ==> x
(make-sum 10 20)
;; ==> 30
(make-sum 'x 'y)
;; ==> (+ x y)
(make-sum 10 'x)
;; ==> (+ 10 x)
(make-sum 'x 10)
;; ==> (+ x 10)

;;
;; Procedures for manipulating products:
;;
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number?  m2)) (* m1 m2))
	(else 
	 (list '* m1 m2))))

(make-product 0 10)
;; ==> 0
(make-product 1 10)
;; ==> 10
(make-product 2 10)
;; ==> 20
(make-product 10 0)
;; ==> 0
(make-product 10 1)
;; ==> 10
(make-product 10 2)
;; ==> 20
(make-product 0 'x)
;; ==> 0
(make-product 1 'x)
;; ==> x 
(make-product 2 'x)
;; ==> (* 2 x)
(make-product 'x 0)
;; ==> 0 
(make-product 'x 1)
;; ==> x
(make-product 'x 2)
;; ==> (* x 2)
(make-product 'x 'y)
;; ==> (* x y)

;;
;; To support differentiation of exponentiation, we also need to define procedures for 
;; manipuating differences:
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
	 
(make-difference 3 0)
;; ==> 3
(make-difference 0 3)
;; ==> -3
(make-difference 4 2)
;; ==> 2
(make-difference 2 4)
;; ==> -2
(make-difference 'x 0)
;; ==> x
(make-difference 0 'x)
;; ==> (* -1 x)
(make-difference 'x 1)
;; ==> (- x 1)
(make-difference 1 'x)
;; ==> (- 1 x)
(make-difference 'x 'y)
;; ==> (- x y)

;;
;; Now let's address the problem statement, and define the supporting procedures
;; needed to support exponentation in the symbolic differentiation procedure.
;;
;; Technically there are three ways we can differentiate an exponentiation:
;;
;; (1) x^n
;; (2) n^x
;; (3) x^x
;;
;; For the sake of simplicity, we will implement only case (1). 
;;
;; For this reason, in the "exponentiation?" test we should check to make sure that 
;; the base is of type variable. We will support both numbers and variables in the 
;; exponent.
;;
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**) (variable? (cadr x))))

(define (base p)
  (cadr p))

(define (exponent p)
  (caddr p))

(define (make-exponentiation base exp-value)
  (cond ((and (number? base) (number? exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(else
	 (list '** base exp-value))))

(make-exponentiation 3 0)
;; ==> 1
(make-exponentiation 3 1)
;; ==> 3
(make-exponentiation 3 2)
;; ==> 9
(make-exponentiation 'x 0)
;; ==> 1
(make-exponentiation 'x 1)
;; ==> x
(make-exponentiation 'x 2)
;; ==> (** x 2)
(make-exponentiation 3 'x)
;; ==> (** 3 x)
(make-exponentiation 'x 'x)
;; ==> (** x x)

;;
;; Finally, define the "deriv" procedure:
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
;; Let's walk through each condition, testing if deriv gives expected answers:
;;
(deriv 3 'x)
;; ==> 0
(deriv 'x 'x)
;; ==> 1
(deriv 'x 'y)
;; ==> 0
(deriv '(+ x y) 'x)
;; ==> 1
(deriv '(+ x y) 'y)
;; ==> 1
(deriv '(+ (* 2 x) y) 'x)
;; ==> 2
(deriv '(+ (* x y) y) 'x)
;; ==> y
(deriv '(+ (* 2 x) y) 'y)
;; ==> 1
(deriv '(+ (* x y) y) 'y)
;; ==> (+ x 1)
(deriv '(- x 1) 'x)
;; ==> 1
(deriv '(- y x) 'x)
;; ==> -1
(deriv '(* x y) 'x)
;; ==> y
(deriv '(** x 3) 'x)
;; ==> (* 3 (** x 2))
(deriv '(** x y) 'x)
;; ==> (* y (** x (- y 1)))

;;
;; Looks like the basics work.
;;
;; Let's step through some of the examples given in the text:
;;
(deriv '(+ x 3) 'x)
;; ==> 1
(deriv '(* x y) 'x)
;; ==> y
(deriv '(+ (* x y) (+ x 3)) 'x)
;; ==> (+ y 1)
(deriv '(+ (* x y) x 3) 'x)
;; ==> (+ y 1)
(deriv '(* (* x y) (+ x 3)) 'x)
;; ==> (+ (* x y) (* y (+ x 3)))

;;
;; Note that this replicates the "response" given in the text, althogh 
;; as indicated, the respnose is not in the "simpelst" possible form. 
;; Specifically, the expression to differentiate was given as:
;;
;;  ==> xy(x+3)
;;  
;;  ==> (x^2)*y + 3xy
;;
;; Differentiating, we obtain:
;;
;;  ==> 2xy + 3y
;;
;; The response given by the symbolic differentiator is:
;;
;;  ==> xy + y(x+3)
;; 
;; Which we can simplify as:
;;
;;  ==> xy + xy + 3y
;;
;;  ==> 2xy + 3y
;;
;; Indeed, the correct answer, but the symbolic differentiator did 
;; not "simplify" the result especially well.
;;