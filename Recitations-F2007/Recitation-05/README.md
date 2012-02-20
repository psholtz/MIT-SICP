Recitation 5 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r05.pdf))
=======================================================================================

Data Structures and Abstractions
-------------------------------- 

**New Procedures**

(a) `(cons a b)` - makes a cons-cell (pair) from a and b.

(b) `(car c)` - extracts the value of the first part of the pair.

(c) `(cdr c)` - extracts the value of the second part of the pair.

(d) `(ca/da/da/dr c)` - shortcuts. `(cadr x)` is the same as `(car (cdr x))`

(e) `(list a b c ...)` - builds a list of the arguments to the procedure.

(f) `(define nil '())` - the special object '(), called the empty list, denotes the end of a list. We often write this as "nil" instead of '().

(g) `(null? a)` - returns #t if a is the empty list (nil or '()), and #f otherwise.

Exercises
---------

(1) Draw box-and-pointer diagrams for the values of the following expressions. Also give the printed representation.
