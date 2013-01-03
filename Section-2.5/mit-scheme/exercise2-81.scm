;;
;; Exercise 2.81
;;
;; Louis Reasoner has noticed that "apply-generic" may try to coerce the arguments 
;; to each other's type even if htey already have the same type. Therefore, he reasons, 
;; we need to put procedures in the coercion table to "coerce" arguments of each type
;; to their own type. For example, in addition to the "scheme-number->complex" coercion
;; shown above, he would do:
;;
;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)
;; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;; (put-coercion 'complex complex complex->complex)
;;
;; (a) With Louis' coercion procedures installed, what happens if "apply-generic"
;; is called with two arguments of type "scheme-number" or two arguments of type
;; "complex" for an operation that is not found in the table for those types? For example,
;; assume that we've defined a generic exponentiation operation:
;;
;; (define (exp x y) (apply-generic 'exp x y))
;;
;; and have put a procedure for exponentiation in the Scheme-number package, but not
;; in any other package.
;;
;; ;; following added to Scheme-number package:
;; (put 'exp '(scheme-number scheme-number)
;;  (lambda (x y) (tag (expt x y))))
;;
;; What happens if we call "exp" with two complex numbers as arguments?
;;
;; [WORKING]
;;

;;
;; First, let's define a "coercion" table:
;;
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;;
;; Let's also define the coercion procedure defined in the text 
;; and install it in the table:
;;
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;;
;; And let's import the definition of "apply-generic" from the text:
;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;
;; Finally, let's define some numbers and see if we can get them to coerce:
;;
(define arg1 (make-scheme-number 1))
;; ==> 1
(define arg2 (make-complex-from-real-imag 2 3))
;; ==> (complex . rectangular 2 . 3)

;;
;; Let's see if it works:
;;
(add arg1 arg2)
;; ==> (complex . rectangular 3 . 3)
(add arg2 arg1)
;; ==> (complex . rectangular 3 . 3)

;;
;; It works, and it works both ways(!)
;;
;; Let's try some other operations:
;;
(sub arg1 arg2)
;; ==> (complex rectangular -1 . -3)
(sub arg2 arg1)
;; ==> (complex rectangular 1 . 3)

(real-part (mul arg1 arg2))
;; ==> 2 
(imag-part (mul arg1 arg2))
;; ==> 3

(real-part (mul arg2 arg1))
;; ==> 2
(imag-part (mul arg2 arg1))
;; ==> 3

(real-part (div arg2 arg1))
;; ==> 2
(imag-part (div arg2 arg1))
;; ==> 3

;;
;; So thus far, everything appears to work just fine, without making Louis' changes.
;;
;; Let's see how these procedures work when combining arguments of the same type:
;;
(add arg1 arg1)
;; ==> 2
(sub arg1 arg1)
;; ==> 0
(mul arg1 arg1)
;; ==> 1
(div arg1 arg1)
;; ==> 1

(add arg2 arg2)
;; ==> (complex rectangular 4 . 6)
(sub arg2 arg2)
;; ==> (complex rectangular 0 . 0)

;;
;; So here too, everything seems to work just fine as well.
;;
;; The reason why is because the procedures for evaluating expressions where both 
;; arguments of the same type have already been included in the operations table.
;; For instance, invoking (add arg1 arg1) causes (in part) the following chain of 
;; expressions to be evaluated:
;;
;; (add arg1 arg1)
;; (add 1 1)
;; (map type-tag '(1 1))
;; (let type-tags '(scheme-number scheme-number)
;; (get 'add '(scheme-number scheme-number))
;;
;; And of course an entry for '(add (scheme-number scheme-number)) was already present
;; in the operations table.
;;
;; The same line of reasoning applies to evaluation of expressions involving two 
;; rational types, or two complex types.
;;

;;
;; Of course, this is not (exactly) what the question is asking us.
;;
;; Let's update the coercion and operations table according to the specifications 
;; of the problem statement, and see whether Louis has a point. 
;;
;; First let's add an exponentiation procedure into the operations table, without
;; doing anything else to the coercion table, and see what happens. We define the 
;; generic math operation:
;;
(define (exp x y) (apply-generic 'exp x y))

;;
;; And modify the operations table for scheme-numbers as follows:
;;
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (p) (= p 0)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done) 

(install-scheme-number-package)
;; ==> done

;;
;; We test out the new procedure:
;;
(exp 2 3)
;; ==> 8
(exp 3 4)
;; ==> 81 

;;
;; It works just fine without making modifications to the coercion table, for the same 
;; reason that the previous examples worked: an entry for '(exp (scheme-number scheme-number))
;; had already been added to the operations table.
;;

;;
;; Conversely, calling (exp arg2 arg2), i.e.., invoking the procedure for two complex 
;; arguments, fails since there is no corresponding entry for '(exp (complex complex))
;; in the operations table.
;;

;;
;; It's unclear why we would need to force coercion between two scheme-number types 
;; in this instance, but let's go ahead and make Louis' modifications and see how 
;; much havor this plays w/ the system:
;;
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;;
;; If we try to evaluate (exp 2 3) again, it works well enough, and gives us the correct response.
;;
;; Again, as before we can trace (loosely) through the call graph to see why it works:
;;
;; (exp 2 3)
;; (apply-generic 'exp 2 3)
;; (map type-tag '(1 1))
;; (let type-tags '(scheme-number scheme-number))
;; (get 'exp '(scheme-number scheme-number))
;;
;; In other words, it works for the same reason why the procedures above work: the entry 
;; for '(exp (scheme-number scheme-number)) is already in the operations table.
;;
;; The problems arise, however, when there is no entry in the operations table for this 
;; particular operation. Before, the "apply-generic" procedure simply threw an exception 
;; in this case, indicating that it could not find the appropriate handler in the operations
;; table. Now, consider what happens:
;;
;; Because it is unable to find a procedure in the operations table corresponding to the 
;; '(exp (complex complex)) tag, it falls into the "second half" of the "apply-generic" 
;; procedure. "type1" and "type2" are both mapped to "complex", and the procedure succeeds
;; in finding a coercion procedure to apply; that is, both "t1->t2" and "t2->t1" are mapped 
;; to the "complex->complex" procedure that was inserted into the coercion table earlier. 
;;
;; The execution thread falls into the first clause of the conditional branch, and applies
;; "t1->t2" to the first complex argument, result in that same argument, and then attempts
;; to recursively (re-)apply the "apply-generic" procedure by invoking "(apply-generic 'exp a1 a2)", 
;; when in fact this was the identical procedure call that we had just made(!)
;;
;; The result is an infinite loop, whereas previously execution had terminated at the "outer" 
;; exception in "apply-generic", when the procedure had been able to identify either a matching
;; entry either in the operations table, or in the coercion table.
;;
;; The danger is that in coercing one type to the same type, we aren't really "coercing" anything.
;;
;; Consequently, the semantics becomes sort of jumbled up, since the recursion in "apply-generic" 
;; presumes that when the recursion is applied to "apply-generic", that the procedure will be 
;; operating with a newly-typed argument, and not just with the same old argument type.
;;

;;
;; (b) Is Louis correct that something had to be done about coercion with arguments of the same type
;; or does "apply-generic" work correctly as is?
;;

;;
;; Louis is not correct.
;;
;; "apply-generic" worked correctly without the modifications he proposed to the coercion table.
;;
;; Applying his modifications to the coercion table causes the possibility of infinite recursions
;; if the operations table is not updated correspondingly, a risk which is not introduced if we 
;; don't use his modifications to the coercion table. Moreover, by "coercing" arguments into the 
;; same type as themselves, he is messing up the semantics of what it means to "coerce" an argument
;; in the first place.
;;

;;
;; (c) Modify "apply-generic" so that it doesn't try coercion if the two arguments have the same type.
;;
(define (apply-generic op . args)
  ;; error handler
  (define (handle-exception type-tags)
    (error "No method for these types: " (list op type-tags)))

  ;; actual procedure
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		;; handle same types case
		(if (equal? type1 type2)
		    (handle-exception type-tags)
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (handle-exception type-tags))))))
	      (handle-exception type-tags))))))

;;
;; Let's clean up the coercion table and try out this new procedure:
;;
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))
(put-coercion 'scheme-number 'complex scheme-number->complex)

;;
;; Running through our unit tests:
;;
(add arg1 arg2)
;; ==> (complex rectangular 3 . 3)
(add arg2 arg1)
;; ==> (complex rectangular 3 . 3)

(sub arg1 arg2)
;; ==> (complex rectangular -1 . -3)
(sub arg2 arg1) 
;; ==> (complex rectangular 1 . 3)

(real-part (mul arg1 arg2))
;; ==> 2
(imag-part (mul arg1 arg2))
;; ==> 3

(real-part (mul arg2 arg1))
;; ==> 2
(imag-part (mul arg2 arg1))
;; ==> 3

(real-part (div arg2 arg1))
;; ==> 2
(imag-part (div arg2 arg1))
;; ==> 3

(add arg1 arg1)
;; ==> 1 
(sub arg1 arg1)
;; ==> 0 
(mul arg1 arg1)
;; ==> 1 
(div arg1 arg1)
;; ==> 1

(add arg2 arg2)
;; ==> (complex rectangular 4 . 6)
(sub arg2 arg2)
;; ==> (complex rectangular 0 . 0)

;;
;; Now let's try the newly added "exp" procedure:
;;
(exp 2 3)
;; ==> 8
(exp 3 4)
;; ==> 81

;; 
;; Again, these work as we anticipate. 
;;
;; How about for complex-complex exponentiation? 
;;
(exp arg2 arg2)
;; ==> [No method for these types: (exp (complex complex))

;;
;; Which is the behavior that we desired.
;;