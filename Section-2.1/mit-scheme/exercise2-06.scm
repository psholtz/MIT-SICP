;;
;; Exercise 2.6
;;
;; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in 
;; a language that can manipulate procedures, we can get by without numbers (at least insofar
;; as nonnegative integers are concerned) by implementing 0 and the opereation of adding 1 as
;;
;; (define zero (lambda (f) (lambda (x) x)))
;; (define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x))))) 
;; 
;; This representation is known as "Church numerals" after its inventor, Alonzo Church, the 
;; logician who invented the lambda calculus.
;;
;; Define "one" and "two" directly (not in terms of "zero" and "add-1") (Hint: Use substitution
;; to evaluate (add-1 zero). Give a direct definition of the addition procedure + (not in terms
;; of repeated application of "add-1").
;;
 
;;
;; First define the procedures "zero" and "add-1":
;;
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;
;; To get a sense for what's happening here, let's experiment a bit with these procedures and 
;; see where it gets us. Looking at the code, both procedures appear to take procedures as 
;; arguments, although in the case of "zero" it isn't obvious that this means anything since
;; no matter what argument we pass to zero, it returns an identity procedure in one argument.
;;
;; "add-1" encapsulates a more complicated behavior, and here the argument "n" to "add-1" is 
;; clearly a procedure in one argument.
;;
;; Let's work with the "inc" and "square" procedures, which are both procedures in one argument.
;;
(define (inc n) (+ n 1))

;;
;; Applying "zero" to a procedure returns a procedure in one argument, which itself is simply 
;; the identity procedure:
;;
(zero inc)
;; ==> #[compound-procedure]

((zero inc) 1)
;; ==> 1
((zero inc) 5)
;; ==> 5
((zero inc) 10)
;; ==> 10
((zero inc) 100)
;; ==> 100

;;
;; Note that this is the case, regardless of which procedure we apply "zero" to:
;;
((zero square) 1)
;; ==> 1
((zero square) 5)
;; ==> 5
((zero square) 10)
;; ==> 10
((zero square) 100)
;; ==> 100

;; 
;; So no matter what procedure we apply "zero" to, it returns a procedure which 
;; itself doesn't really do anything. "zero" is, perhaps, an apt name for this procedure.
;;

;;
;; Applying "zero" to "add-1" will return a procedure in one argument:
;;
(add-1 zero)
;; ==> #[compound-procedure]

;;
;; Judging from the source code for "add-1", the procedure returned consumes a procedure in 
;; one argument, which itself returns a third procedure in one argument. Let's try to apply 
;; (add-1 zero) to one-argument procedures like "inc" and "square", and see what happens:
;;
((add-1 zero) inc)
;; ==> #[compound-procedure]

;;
;; Again a procedure in one argument is returned. 
;;
;; Let's apply this procedure to various numbers:
;;
(((add-1 zero) inc) 1)
;; ==> 2
(((add-1 zero) inc) 5)
;; ==> 6
(((add-1 zero) inc) 10)
;; ==> 11
(((add-1 zero) inc) 100)
;; ==> 101

;;
;; Clearly, by "add-1"-ing to "zero", we end up applying the "inc" procedure just once.
;;
;; Let's see what happens if we apply the generated compound procedure to "square":
;;
(((add-1 zero) square) 1)
;; ==> 1
(((add-1 zero) square) 5)
;; ==> 25
(((add-1 zero) square) 10)
;; ==> 100
(((add-1 zero) square) 100)
;; ==> 10000

;;
;; If we apply "add-1" twice, it is reasonbly to presume that the resulting compound 
;; procedure will apply its argument procedure twice:
;;
(((add-1 (add-1 zero)) inc) 1)
;; ==> 3
(((add-1 (add-1 zero)) inc) 5)
;; ==> 7
(((add-1 (add-1 zero)) inc) 10)
;; ==> 12
(((add-1 (add-1 zero)) inc) 100)
;; ==> 102

;;
;; The same results are obtained when applying the compound-procedure to "square", although 
;; here the result is not the natural "succession of numerals" that we obtain when using 
;; the "increment" procedure:
;;
(((add-1 (add-1 zero)) square) 2)
;; ==> 16
(((add-1 (add-1 zero)) square) 3)
;; ==> 81

;;
;; If we were lazy, we can do what the book instructs us NOT to do, and that is define procedures
;; "one" and "two" in terms of "add-1" and "zero":
;;
(define one (add-1 zero))
(define two (add-1 (add-1 zero)))

;;
;; We would then have:
;;
((one inc) 0)
;; ==> 1
((two inc) 0)
;; ==> 2

;;
;; But let's do instead what the book recommends that we do, and expend (add-1 zero) using 
;; the substitution model, to gain a better understanding of what's happening:
;;
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))

;;
;; For any "f", the procedure (zero f) simply returns the identity procedure:
;;
(zero f)
(lambda (g) (lambda (y) y))
(lambda (y) y)

;;
;; So we can replace "(zero f)" with "(lambda (y) y)":
;;
(lambda (f) (lambda (x) (f ((lambda (y) y) x))))
(lambda (f) (lambda (x) (f x)))

;;
;; This is as far as we can reduce the procedure, without supplying an argument procedure 
;; (in one variable). Let's try applying "inc":
;;
((lambda (f) (lambda (x) (f x))) inc)
(lambda (x) (inc x))

;; 
;; So evaluation of the expression:
;;
((add-1 zero) inc)

;; 
;; Yields the expression:
;;
(lambda (x) (inc x))

;;
;; In other words, it results in one application of "inc"
;;

;;
;; Let's try to expand (add-1 (add-1 zero)) by the substitution model:
;;
(add-1 (add-1 zero))
(lambda (f) (lambda (x) (f (((add-1 zero) f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
(lambda (f) (lambda (x) (f (f x))))

;;
;; Again, this is as far as we can take the problem, without supplying the 
;; argument procedure (in one variable), but clearly, here we have a procedure
;; applies its argument procedure twice.
;;
;; Let's apply it to "inc" to verify this:
;;
((lambda (f) (lambda (x) (f (f x)))) inc)
(lambda (x) (inc (inc x)))

;;
;; Finally, let's apply this to say, "0":
;;
((lambda (x) (inc (inc x))) 0)
(inc (inc 0))
(inc 1)
2

;;
;; From what we have determined, it's quite simply to define "one" and "two" directly.
;; We simply apply the argument procedure once, or twice:
;;
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;;
;; Let's see if these procedures still work the way we anticipate, when applied to "inc":
;;
((one inc) 0)
;; ==> 1
((two inc) 0)
;; ==> 2