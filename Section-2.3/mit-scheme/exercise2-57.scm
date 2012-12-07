;;
;; Exercise 2.57
;;
;; Extend the differentiation program to handle sums and products of arbitrary numbers of (two or
;; more) terms. Then the last example above could be expressed as:
;; 
;;  (deriv '(* x y (+ x 3)) 'x)
;;
;; Try to do this by changing only the representation for sums and products, without changing the 
;; "deriv" procedure at all. For example, the "addend" of a sum would be the first term, and the 
;; "augend" would be the sum of the rest of the terms.
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
;; Let's first verify that the existing procedure doesn't handle list structures
;; with more than two arguments (not that we would expect it to, but still):
;;
(deriv '(+ x x x) 'x)
;; ==> 2

;;
;; Indeed, the "deriv" procedure does not appear to "see" the last x in this structure.
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
;; Let's go back and see whether how our initial use case performs using these modifications:
;;
(deriv '(+ x x x) 'x)
;; ==> 3

;;
;; Nice! The correct answer..
;;
;; Let's try the same thing using multiplication:
;;
(deriv '(* x y z) 'x)
;; ==> (* y z)

;;
;; Again, correct.
;;
;; In fact, this is the only change we need to make to have a fully-functioning symbolic 
;; differentiator, although there are two areas where we might still be able to make 
;; improvements: (1) when inputting information to the differentiatior, there is no way 
;; at present to "mechanically" enter a list with more than two elements. That is to say,
;; we can write and have the differentiator symbolically evaluate an expression as thus:
;;
(deriv '(* x y z) 'x)
;; ==> '(* y z)

;; 
;; But there is no way to enter an expression like:
;;
;; (deriv (make-product 'x 'y 'z) 'x)
;;
;; since "make-product" at present still only takes two arguments. Indeed, "make-sum" and 
;; "make-product" do not need to be modified to support the new requirements, since, 
;; given the recursive nature of "deriv", and the recursive manner in which "augend" and 
;; "multiplicand" have been redefined, the two procedures "make-sum" and "make-product" 
;; never need to evaluate more than two expressions at a time to symbolically differentiate
;; the given expression.
;;
;; From a user standpoint, such a facility might be nice. That way, the user doesn't need to 
;; concern themselves with the low-level details of how products or sums are represented 
;; (i.e., as lists, or otherwise), and can simply use the API to generate products, sums, etc.
;;
;; The second point, (2), related somewhat to the first one, is that given the current 
;; architecture, it's not difficult to generate "messy" responses from the differentiator.
;; Indeed, "cleaning up" and "simplifying" the responses generated by the differentiator
;; could be a full-fledged project on its own, but perhaps there are a few simple things 
;; we can do here to boot-strap that process, and make the differentiator's resulting 
;; output more readable.
;;

;;
;; For instance, suppose we try to evaluate the (deliberately obscure) expression:
;;
(deriv '(+ y z 5 (* 2 x t 5)) 'x)
;; ==> (* 2 (* t 5))

;;
;; The interpreter responds with a "corect", albeit somewhat obscured response.
;;

;;
;; Let's try to improve on this state of affairs by redefining "make-sum" and "make-product"
;; for user input, and in such a way that will "clean up" input going into the differentiator
;; so as to render the resulting output in a more readable state.
;;
;; We won't actually modify "make-sum" and "make-product", since, as indicated above, this 
;; is unnecessary. Given the recursive nature of "deriv", "augend" and "multiplicand", the 
;; two argument "make-sum" and "make-product" procedures will work just fine.
;;
;; Rather, we are trying to redefine "sum" and "product" methods for end-user use, and 
;; to do so in a way that cleans up the input for easier reading when the differentiator 
;; terminates.
;;

;; 
;; We will need several supporting methods: (a) one to extract the symbols from an argument
;; list; and (b) one to extract the numbers from an argument list.
;;
(define (get-symbol-list elems)
  (filter (lambda (x) (not (number? x))) elems))
(define (get-number-list elems)
  (filter (lambda (x) (number? x)) elems))

;;
;; Let's test these out:
;;
(define x '(1 2 a b 3 4 c d))
(get-symbol-list x)
;; ==> (a b c d)
(get-number-list x)
;; ==> (1 2 3 4)

;;
;; We also need a more sophisticated procedure, which checks to see whether 
;; the argument list contains only 1 number, and whether that number is the 
;; argument "n":
;;
(define (is-n-the-only-number? n elems)
  (let ((numbers (get-number-list elems)))
    (if (null? numbers)
	#f
	(and (= (car numbers) n) (null? (cdr numbers))))))

(is-n-the-only-number? 1 '())
;; ==> #f
(is-n-the-only-number? 1 '(1))
;; ==> #t
(is-n-the-only-number? 1 '(1 a b))
;; ==> #t
(is-n-the-only-number? 1 '(1 a b 1))
;; ==> #f 
(is-n-the-only-number? 1 '(1 a b 2))
;; ==> #f
(is-n-the-only-number? 3 '(1 a b 2))
;; ==> #f

;;
;; We define "make-sum-n" to take 0 or more arguments (i.e., n arguments):
;;
(define (make-sum-n . elems)
  (cond ((null? elems) 0)
	((null? (cdr elems)) (car elems))
	(else
	 (let ((numbers (get-number-list elems))
	       (symbols (get-symbol-list elems)))	   
	   (cond ((null? symbols) (apply + elems))
		 ((> (length numbers) 1)
		  (apply make-sum-n
			 (append symbols
				 (list (apply + numbers)))))
		 ((is-n-the-only-number? 0 elems)
		  (apply make-sum-n symbols))
		 (else
		  (append '(+) elems)))))))

;;
;; Let's run some simple unit tests:
;;
(make-sum-n 0 0)
;; ==> 0
(make-sum-n 8 0 3 0 4 0 5 0)
;; ==> 20
(make-sum-n 'x 'y 'z)
;; ==> (+ x y z)
(make-sum-n 'x 0 'y)
;; ==> (+ x y)
(make-sum-n 'x 0 1 0 3 4 0 'y)
;; ==> (+ x y 8)
(make-sum-n 'x 2 3 (make-exponentiation 'y 2) 4)
;; ==> (+ x (** y 2) 9)
(make-sum-n)
;; ==> 0
(make-sum-n 3)
;; ==> 3
(make-sum-n 'a)
;; ==> a
(make-sum-n 1 2 3)
;; ==> 6
(make-sum-n 1 'a 'b)
;; ==> (+ 1 a b)
(make-sum-n 3 'a 'b)
;; ==> (+ 3 a b)
(make-sum-n 3 'a 'b 4 'c)
;; ==> (+ a b c 7)
(make-sum-n 0 'a 'b)
;; ==> (+ a b)
(make-sum-n 0 'a 'b 0)
;; ==> (+ a b)
(make-sum-n 'a 'b 'c)
;; ==> (+ a b c)

;;
;; We can define "make-product-input" in nearly the identical way:
;;
(define (make-product-n . elems)
  (cond ((null? elems) 1)
	((null? (cdr elems)) (car elems))
	(else
	 (let ((numbers (get-number-list elems))
	       (symbols (get-symbol-list elems)))
	   (cond ((null? symbols) (apply * elems))
		 ((> (length numbers) 1)
		  (apply make-product-n
			 (append symbols
				 (list (apply * numbers)))))
		 ((is-n-the-only-number? 0 elems) 0)
		 ((is-n-the-only-number? 1 elems)
		  (apply make-product-n symbols))
		 (else 
		  (append '(*) elems)))))))

(make-product-n 1 1)
;; ==> 1
(make-product-n 10 9 0 1)
;; ==> 0
(make-product-n 'x 0 'y 4)
;; ==> 0
(make-product-n 'x 'y 'z)
;; ==> (* x y z)
(make-product-n 'x 'y 'z 0)
;; ==> 0
(make-product-n 1 'x 1 'y 'z)
;; ==> (* x y z)
(make-product-n 'x 2 3 (make-exponentiation 'y 2) 4)
;; ==> (* x (** y 2) 24)
(make-product-n)
;; ==> 1
(make-product-n 3)
;; ==> 3
(make-product-n 'a)
;; ==> a
(make-product-n 1 2 3)
;; ==> 6
(make-product-n 1 'a 'b)
;; ==> (* a b)
(make-product-n 3 'a 'b)
;; ==> (* 3 a b)
(make-product-n 3 'a 'b 4 'c)
;; ==> (* a b c 12)
(make-product-n 0 'a 'b)
;; ==> 0
(make-product-n 0 'a 'b 0)
;; ==> 0
(make-product-n 1 'a 'b)
;; ==> (* a b)
(make-product-n 1 'a 'b 1)
;; ==> (* a b)
(make-product-n 'a 'b 'c)
;; ==> (* a b c)

;;
;; There is a temptation here to "abstract" out some of the internals of the 
;; two methods, much as we did for "augend" and "multiplicand", but in fact 
;; the fact structures are not completely congruent, so we won't go into that
;; exercise here.
;;

;;
;; At any rate, let's try to recompute the example derivative we had used earlier:
;;
(deriv (make-sum-n 'y 'z 5 (make-product-n 2 'x 't 5)) 'x)
;; ==> (* t 10)

;;
;; Which, indeed, is a much more readable way of rendering the answer.
;;

;;
;; Let's try the example cited in the text:
;;
(deriv '(* x y (+ x 3)) 'x)
;; ==> (+ (* x y) (* y (+ x 3)))

;; 
;; For completeness, let's walk through some of the unit tests we 
;; did in the previous exercise:
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
(deriv '(+ x y) 'z)
;; ==> 0
(deriv '(+ (* 2 x) y) 'x)
;; ==> 2
(deriv '(+ (* 2 x) y) 'y)
;; ==> 1
(deriv '(+ (* x y) y) 'x)
;; ==> y
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
;; Further references on this problem set are available at:
;;
;;  http://wqzhang.wordpress.com/sicp-solutions/
;;  http://wiki.drewhess.com/wiki/Category:SICP_solutions
;;