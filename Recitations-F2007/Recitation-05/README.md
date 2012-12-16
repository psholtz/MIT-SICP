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

(a) `(cons 1 2)`

(b) `(cons 1 (cons 3 (cons 5 '())))`

(c) `(cons (cons (cons 3 2) (cons 1 0)) '())`

(d) `(cons 0 (list 1 2))`

(e) `(list (cons 1 2) (list 4 5) 3)`

(2) Write expressions whose values will print out like the folllowing:

(a) `(1 2 3)`

(b) `(1 2 . 3)`

(c) `((1 2) (3 4) (5 6))`

(3) Create a data abstraction for points in a place. It should have a constructor `(make-point x y)`, which returns a point, and two selectors `(point-x pt)` and `(point-y pt)`, which return the x and y coordinates.

(4) Now, extend the point abstraction to handle line segments, with a constructor `(make-line-segment p1 p2)` and selectors **line-segment-start** and **line-segment-end**.

(5) Write a procedure `(intersection seg1 seg2)` that returns a point where two line segments intersect if they do, and returns **#f** if they do not intersect. Be sure to honor the abstractions defined.
