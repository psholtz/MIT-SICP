;;
;; Exercise 2.55
;;
;; Eva Lu Ator types into the interpreter the expression:
;;
;;  (car ''abracadabra)
;;
;; To her surprise, the interpreter prints back "quote". Explain.
;;

;; 
;; The interpreter will expand 'a to (quote a), thus:
;;

'a
;; ==> a

(quote a)
;; ==> a

(eq? 'a (quote a))
;; ==> #t

;;
;; 'a and (quote a) represent the same object in memory, which object is represented by 
;; the interpreter onscreen as "a" and which is why (eq? 'a (quote a)) evaluates to true. 
;; They are, of course, also "equal?" in the more general sense of having "congruent" structure:
;;
(equal? 'a (quote a))
;; ==> #t

;;
;; As indicated in the text, "quote" allows us represent compound objects as lists thusly:
;;
'(1 2 3)
;; ==> (1 2 3)
'(a b c)
;; ==> (a b c)
'(quote a)
;; ==> (quote a)

;;
;; Each of these expressions evaluates to a traditional Lisp list. 
;;
;; The last expression, in particular, evaluates to a traditional Lisp list where the 
;; first element of the list is the symbol "quote", and the second element of the list
;; is the symbol "a":
;;
(define m '(quote a))

(pair? m)
;; ==> #t
(list? m)
;; ==> #t
(car m)
;; ==> quote
(cadr m)
;; ==> a 
(symbol? (car m))
;; ==> #t
(symbol? (cadr m))
;; ==> #t

;;
;; Now, to answer the question: the expressions ''a and '(quote a) and (quote (quote a))
;; all evaluate to the same result, which is the Lisp list (quote a), where the first element
;; of the list is the symbol "quote" and the second element of the list is the symbol "a":
;;
''a
;; ==> (quote a)

'(quote a)
;; ==> (quote a)

(quote (quote a))
;; ==> (quote a)

(list? ''a)
;; ==> #t

;;
;; The interpreter expands ''a into the expression (quote (quote a)).
;;
;; When it encounters this expression, it "applies" the function "quote" to the 
;; argument, which in this case is the structure (quote a). In other words, 
;; the interpeter understands the second "quote" to simply be a symbol which is 
;; being included as part of a list. 
;;
;; The first "quote" is used to create the list structure, and the second quote 
;; is simply a symbol which is included in that list structure.
;;
;; It would work the same way, for instance, if we wrote (quote (car a)). This, too
;; would generate a list where the first element was the symbol "car" and the second
;; element was "a":
;;
(quote (car a))
;; ==> (car a)

(list? (quote (car a)))
;; ==> #t

(car (quote (car a)))
;; ==> car