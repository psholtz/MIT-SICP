;;
;; Exercise 4
;;
;; Suppose x is bound to the list (1 2 3 4 5 6 7). Using "map", "filter" and/or "fold-right", write 
;; an expression involving "x" that returns:
;;
;; (a) (1 4 9 16 25 36 49)
;;
(define x (list 1 2 3 4 5 6 7))

(map square x)

;;
;; (b) (1 3 5 7)
;;
(filter odd? x)

;;
;; (c) ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))
;;

;;
;; One simple possibility is the following:
;;
(map (lambda (y) (list y y)) x)

;;
;; or the even more pathological:
;;
(map (lambda (x) (list x x)) x)

;;
;; (d) ((2) ((4) ((6) ())))
;;

;; 
;; First let's break down what this expression actually "is":
;;
(list (list 2) (list (list 4) (list (list 6) '())))
;; ==> ((2) ((4) ((6) ())))

;;
;; Seeing what the pattern is, we can construct a function f that generates the structure we desire:
;;
(define (f x)
  (let ((a (filter even? x)))
    (define (f-iter b)
      (if (null? b)
	  '()
	  (list (list (car b)) (f-iter (cdr b)))))
    (f-iter a)))

;;
;; Running the procedure:
;;
(f x)
;; ==> ((2) ((4) ((6) ())))

;;
;; Which is what we were looking for.
;;
;; However, this method uses a custom-defined procedure, rather than the predicates we were furnished with.
;; 
;; Let's see if we can rewrite it using the "fold-right" procedure:
;;
(fold-right 
 (lambda (a b) (list (list a) b)) 
 '()
 (filter (lambda (y) (even? y)) x))

;; ==> ((2) ((4) ((6) ())))     

;;
;; Which again, is the answer we are seeking.
;;

;;
;; The reasons this work is that we first extract the "even" numbers using the filter procedure:
;;
(filter (lambda (y) (even? y)) x)
;; ==> (2 4 6)

;;
;; And then we "fold from the right" using the custom-defined anonymous function 
;; (lambda (a b) (list (list a b) b)) to generate the final result.
;;

;;
;; It is instructive to step through the call graph:
;;
(fold-right (lambda (a b) (list (list a) b)) '() '(2 4 6))

((lambda (a b) (list (list a) b)) 
 2 
 (fold-right (lambda (a b) (list (list a) b)) '() '(4 6)))

((lambda (a b) (list (list a) b)) 
 2
 ((lambda (a b) (list (list a) b)) 4 (fold-right (lambda (a b) (list (list a) b)) '() '(6))))

((lambda (a b) (list (list a) b))
 2
 ((lambda (a b) (list (list a) b)) 
  4
  ((lambda (a b) (list (list a) b))
   6
   (fold-right (lambda (a b) (list (list a) b)) '() '()))))

((lambda (a b) (list (list a) b))
 2
 ((lambda (a b) (list (list a) b))
  4
  ((lambda (a b) (list (list a) b))
   6 '())))

((lambda (a b) (list (list a) b))
 2
 ((lambda (a b) (list (list a) b))
  4
  (list (list 6) '())))

((lambda (a b) (list (list a) b))
 2
 ((lambda (a b) (list (list a) b))
  (list (list 4) (list (list 6) '()))))

((lambda (a b) (list (list a) b))
 2
 (list (list 4) (list (list 6) '())))

(list (list 2) (list (list 4) (list (list 6) '())))

((2) ((4) ((6) ())))

;;
;; (e) The maximum element of x: 7
;;

;;
;; Let's walk down the list structure using "fold-right" and compare each two successive
;; elements, saving the largest one. We need to "initialize" the algorithm using a number
;; we know will be "smaller" than the smallest element in the list. In C/C++ we might set
;; this variable to something like (-1) * MAX_INT or some such value. 
;;
;; Here, let's just set this sentinel value to something like -1000, since we know that all 
;; the elements in the list will be larger than this value:
;;
(fold-right (lambda (a b) (if (> a b) a b))
	    -1000
	    x)

;; ==> 7

;;
;; Playing around with this procedure on other lists shows that it returns the maximum 
;; element (even if that element is not the last element in the last structure), so long 
;; as the "minimum sentinel" value is correctly initialized.
;;

;;
;; Again, it is instructive to step through the call graph:
;;
(define func-e (lambda (a b) (if (> a b) a b)))

(fold-right func-e -1000 '(1 2 3 4 5 6 7))
(func-e 1 (fold-right func-e -1000 '(2 3 4 5 6 7)))
(func-e 1 (func-e 2 (fold-right func-e -1000 '(3 4 5 6 7))))
(func-e 1 (func-e 2 (func-e 3 (fold-right func-e -1000 '(4 5 6 7)))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (fold-right func-e -1000 '(5 6 7))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 (fold-right func-e -1000 '(6 7)))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 (func-e 6 (fold-right func-e -1000 '(7))))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 (func-e 6 (func-e 7 (fold-right func-e -1000 '()))))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 (func-e 6 (func-e 7 -1000)))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 (func-e 6 7))))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 (func-e 5 7)))))
(func-e 1 (func-e 2 (func-e 3 (func-e 4 7))))
(func-e 1 (func-e 2 (func-e 3 7)))
(func-e 1 (func-e 2 7))
(func-e 1 7)
7

;;
;; (f) A list of the last element of x: (7)
;;

;;
;; One invocation which generates the desired response, using fold-right, is:
;;
(list (fold-right (lambda (a b) (if (null? b) a b)) '() x))

;; ==> (7)

;;
;; Again, it is useful to step through the call graph, or at least that part of which 
;; which involves the "fold-right" procedure:
;;
(define func-f (lambda (a b) (if (null? b) a b)))

(fold-right func-f '() '(1 2 3 4 5 6 7))
(func-f 1 (fold-right func-f '() '(2 3 4 5 6 7)))
(func-f 1 (func-f 2 (fold-right func-f '() '(3 4 5 6 7))))
(func-f 1 (func-f 2 (func-f 3 (fold-right func-f '() '(4 5 6 7)))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (fold-right func-f '() '(5 6 7))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 (fold-right func-f '() '(6 7)))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 (func-f 6 (fold-right func-f '() '(7))))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 (func-f 6 (func-f 7 (fold-right func-f '() '()))))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 (func-f 6 (func-f 7 '())))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 (func-f 6 7))))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 (func-f 5 7)))))
(func-f 1 (func-f 2 (func-f 3 (func-f 4 7))))
(func-f 1 (func-f 2 (func-f 3 7)))
(func-f 1 (func-f 2 7))
(func-f 1 7)
7

;;
;; Another way this can be done is to use the "fold-left" procedure:
;;
(fold-left 
 (lambda (a b)
   (if (null? a)
       (list b)
       '()))
 '()
 x)

;; ==> (7)

;;
;; As an aside, it is note that we can obtain the "first" element in the list using a procedure 
;; like this:
;;
(fold-right 
 (lambda (a b)
   (if (null? b)
       (list a)
       '()))
 '()
 x)

;; ==> (1)

;;
;; The utility of this approach is dubious, given the existence of the "car" procedure, but is
;; cited here nevertheless.
;;

;;
;; (g) The list in reverse order: (7 6 5 4 3 2 1)
;;

(define (reverse items)
  (fold-right (lambda (a b) (append b (list a))) '() items))

(reverse x)
;; ==> (7 6 5 4 3 2 1)

;;
;; Let's step through the call graph, to see how much time is involved in this procedure call:
;;
(define func-g (lambda (a b) (append b (list a))))

(fold-right func-g '() '(1 2 3 4 5 6 7))
(func-g 1 (fold-right func-g '() '(2 3 4 5 6 7)))
(func-g 1 (func-g 2 (fold-right func-g '() '(3 4 5 6 7))))
(func-g 1 (func-g 2 (func-g 3 (fold-right func-g '() '(4 5 6 7)))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (fold-right func-g '() '(5 6 7))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (fold-right func-g '() '(6 7)))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (func-g 6 (fold-right func-g '() '(7))))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (func-g 6 (func-g 7 (fold-right func-g '() '()))))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (func-g 6 (func-g 7 '())))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (func-g 6 (append '() '(7))))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (func-g 6 '(7)))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 (append '(7) '(6)))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (func-g 5 '(7 6))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 (append '(7 6) '(5))))))
(func-g 1 (func-g 2 (func-g 3 (func-g 4 '(7 6 5)))))
(func-g 1 (func-g 2 (func-g 3 (append '(7 6 5) '(4)))))
(func-g 1 (func-g 2 (func-g 3 '(7 6 5 4))))
(func-g 1 (func-g 2 (append '(7 6 5 4) '(3))))
(func-g 1 (func-g 2 '(7 6 5 4 3)))
(func-g 1 (append '(7 6 5 4 3) '(2)))
(func-g 1 '(7 6 5 4 3 2))
(append '(7 6 5 4 3 2) '(1))
'(7 6 5 4 3 2 1)

;;
;; The list size is 7, and a total of 23 steps are involved. 
;;
;; Calls to the procedure "append" are made 7 times.
;;
;; If we increase n to 8, a total of 3 steps are added, as well as 1 additional call to "append":
;;
(func-g 7 (fold-right func-g '() '(8)))
(func-g 7 (func-g 8 (fold-right func-g '() '())))
(func-g 7 (func-g 8 '()))
(func-g 7 (append '() '(8)))
(func-g 7 '(8))

;;
;; If we invoke the method with a list of size 1, a total of 5 steps, 
;; including 1 call to "append" are made:
;;
(fold-right func-g '() '(1))
(func-g 1 (fold-right func-g '() '()))
(func-g 1 '())
(append '() '(1))
'(1)

;;
;; We surmise that the total amount of time required to execute the procedure is 3N+5 steps, 
;; where N is the size of the list to be reversed, as well as N calls to "append".
;; We now calculate the cost of invoking "append":
;;

;;
;; The procedure "append" is defined as follows:
;;
(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1)
	    (append (cdr lst1) lst2))))

;;
;; There are 4 steps involved in calling (append '(7) '(6)):
;;
(append '(7) '(6))
(cons 7 (append '() '(6)))
(cons 7 '(6))
'(7 6)

;;
;; There are 14 steps involved in calling (append '(7 6 5 4 3 2) '(1)):
;;
(append '(7 6 5 4 3 2) '(1))
(cons 7 (append '(6 5 4 3 2) '(1)))
(cons 7 (cons 6 (append '(5 4 3 2) '(1))))
(cons 7 (cons 6 (cons 5 (append '(4 3 2) '(1)))))
(cons 7 (cons 6 (cons 5 (cons 4 (append '(3 2) '(1))))))
(cons 7 (cons 6 (cons 5 (cons 4 (cons 3 (append '(2) '(1)))))))
(cons 7 (cons 6 (cons 5 (cons 4 (cons 3 (cons 2 (append '() '(1))))))))
(cons 7 (cons 6 (cons 5 (cons 4 (cons 3 (cons 2 '(1)))))))
(cons 7 (cons 6 (cons 5 (cons 4 (cons 3 '(2 1))))))
(cons 7 (cons 6 (cons 5 (cons 4 '(3 2 1)))))
(cons 7 (cons 6 (cons 5 '(4 3 2 1))))
(cons 7 (cons 6 '(5 4 3 2 1)))
(cons 7 '(6 5 4 3 2 1))
'(7 6 5 4 3 2 1)

;;
;; We surmise that invoking (append a b), where (count a) == n and (count b) == 1, 
;; will involved 2n + 2 steps. The total number of additional steps that calling 
;; "append" incurs, will sum from k=1 to k=(n-1) where the summand is 2k+2:
;;
;; Total additional calls: Sum(k=1, k=n-1) (2k+2)
;;
;; Working out this summation, we arrive at: 
;; 
;;  Sum = 2 * (1/2) * n * (n+1) + 2*n - 2
;;  Sum = n^2 + 3n - 2
;; 
;; This sum must now be added to the 3n+5 steps involved in the "outer" computation, 
;; leading to a grand total number of steps involved as:
;;
;;  Total = n^2 + 3n - 2 + 3n + 5 
;;  Total = n^2 + 6n + 3
;;
;; In other words, the procedure executes in O(n^2) time.
;;

;;
;; (h) Bonus: reverse a list in less than O(n^2) time.
;;

;; 
;; The trick to getting a list reversal algorithm working in less than O(n^2) time
;; (or even, ideally, in linear time), is to do away with the call to "append", which 
;; is where the "performance hit" we are currently experiencing is taking place.
;;
;; In the implementation that follows, we replace the call with "append" with a call 
;; to "cons", which is much more efficient. The procedure uses an iterative "running 
;; total" of the reversed list that has been constructed thus far (i.e., "lst2").
;;
(define (reverse items)
  (define (reverse-iter lst1 lst2)
    (if (null? lst1)
	lst2
	(reverse-iter (cdr lst1) (cons (car lst1) lst2))))
  (reverse-iter items '()))

;;
;; Run some unit tests:
;;
(reverse x)
;; ==> '(7 6 5 4 3 2 1)

;;
;; Let's expand the call graph, to see just what is taking place here:
;;
(reverse '(1 2 3))
(reverse-iter '(1 2 3) '())
(reverse-iter '(2 3) '(1))
(reverse-iter '(3) '(2 1))
(reverse-iter '() '(3 2 1))
'(3 2 1)

;;
;; Clearly, the algorithm executes in O(n) time, and much more rapidly than the previous definition of reverse.
;;