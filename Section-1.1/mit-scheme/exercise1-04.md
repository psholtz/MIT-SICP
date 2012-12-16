Exercise 1.4
============

Observe that our model of evaluation allows for combinations whose operators are compound expressions. 

Use this observation to describe the behavior of the following procedure:

```scheme
(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b))
```

Solution
-------- 

If b is positive, the `if` predicate selects the + operator.

Hence, if b is positive, the procedure simply adds a to b.

Otherwise, if b is zero or negative, the `if` predicate selects the - operator. In this case, the procedure subtracts b from a. 

The result, in either case, is to add the absolute value of b to a.

