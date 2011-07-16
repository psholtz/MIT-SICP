Exercise 1.5
============

Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

<pre>
(define (p) (p))

(define (test x y)
	(if (= x 0)
		0 
		y))
</pre>

Then he evaluates the expression:

<pre>
(test 0 (p))
</pre>

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer? (Assume that the evaluation rule for the special form `if` is the same whether the interpreter is using normal or applicative order: the predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression).

Solution
--------

In an applicative-order evaluation model, the interpreter first evaluates the operator and operands, and then applies the resulting procedure to the resulting arguments. There is a risk that recursively-defined procedures may fall into infinite recursions, which, indeed, is exactly what happens here. Using an applicative-order interpreter, Ben's `test` procedure would be expanded and interpreted roughly as follows:

<pre>
(test 0 (p))
(if (= 0 0) 0 (p))
(if (= 0 0) 0 (p))
(if (= 0 0) 0 (p))
...
</pre>

In other words, an interpreter that uses applicative-order evaluation will first evaluate the operator, expanding it down to the primitive `if`, and it will attempt to evaluate each of the operands, which in this case are 0 and (p). Since (p) itself is defined simply as (p), the interpreter continues trying to expand this expression, ad-infinitum.

In a normal-order evaluation model, the interpreter does not evaluate the operands until their value is actually needed. Ben's `test` procedure would be expanded and interpreted roughly as follows:

<pre>
(test 0 (p))
(if (= 0 0) 0 (p))
</pre>

The expansion stops here since `if` is a primitive operator. Since the predicate (= 0 0) evaluates to true, the procedure returns 0 and terminates. 
