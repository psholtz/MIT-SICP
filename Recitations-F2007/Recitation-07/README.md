Recitation 7 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r07.pdf))
=======================================================================================

List Functions
-------------- 

```scheme
(define (length lst)
 (if (null? lst)
  0
  (+ 1 (length (cdr lst)))))

(define (map proc lst)
 (if (null? lst)
  '()
  (cons (proc (car lst))
        (map proc (cdr lst)))))

(define (filter pred lst)
 (if (null? lst)
  '()
  (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

;; also known as "accumulate", "foldr"
(define (fold-right op init lst)
 (if (null? lst)
  init
  (op (car lst)
      (fold-right op init (cdr lst)))))

(define (list-ref lst n)
 (if (= n 0)
     (car lst)
     (list-ref (cdr lst) (- n 1))))

(define (append lst1 lst2)
 (if (null? lst1)
  lst2
  (cons (car lst)
        (append (cdr lst1) lst2))))
```

Exercises
---------

(1) Write a function "occurrences" that takes a number and a list and counts the number 
    of times the number appears in the list. Write two version -- one that uses "filter" and 
    one that uses "fold-right". For example:

```scheme
(occurrences 1 (list 1 2 1 1 3)) ==> 3
</pre>

(2) Define "length" using a higher order list procedure.

(3) Define "ls" to be a list of "procedures":

<pre>
(define (square x) (* x x))
(define (double x) (* x 2))
(define (inc x) (+ x 1))
(define ls (list square double inc))
```

Now say we want a function "apply-procs" that behaves as follows:

```scheme
(apply-procs ls 4)
==> ((square 4) (double 4) (inc 4)) ==> (16 8 5)
(apply-procs ls 3)
==> ((square 3) (double 3) (inc 3)) ==> (9 6 4)
```

Write a definition for "apply-procs" using "map".

(4) Suppose "x" is bound to the list (1 2 3 4 5 6 7). Using "map", "filter" and/or "fold-right", 
    write an expression involving "x" that returns:

(a) (1 4 9 16 25 36 49)

(b) (1 3 5 7)

(c) ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))

(d) ((2) ((4) ((6) ())))

(e) The maximum element of x: 7

(f) List of last element of x: (7)

(g) The list in reverse order: (7 6 5 4 3 2 1)

(h) Bonus: reverse a list in less than O(n^2) time.
