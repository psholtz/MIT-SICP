Section 1.1
=========== 

Naming
------ 

Naming provides a means of abstraction. Computational objects may be extremely complex, and it would be cumbersome to have to remember and repeat their details each time we want to use them. The ability to create name-object pairs incrementally, in successive interactions makes the construction of large programs easier and more convinient.

The possibility of associating values with symbols and later retrieving them means that the interpreter must maintain some sort of memory that keeps track of the name-object pairs. This memory is called the **environment** (more precisely, the global environment, since we will see later that a computation may involve a number of different environments). 

Evaluating Combinations
-----------------------

Lisp encourages developers to think about computation in a "procedural" way. However, consider that in evaluating combinations, the interpreter itself is following a (recursive) procedure:

1. The interpreter first evaluates the subexpressions of the combination.
2. The interpreter then applies the procedure that is the value of the left-most subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

The evaluation rule is therefore recursive, in that in order to accomplish the evaluation process for a combination, we must first perform the evaluation process for each element of the combination.

Applicative-Order Evaluation
---------------------------- 

One of the main concepts in Section 1.1 is to introduce the difference between "**applicative-order**" evaluation and "**normal-order**" evaluation.

Applicative-order evaluation is the model used by production Scheme interpreters, largely for reasons of performance and efficiency. Nevertheless, normal-order evaluation can be useful as a heuristic tool, and its various uses are explored in later chapters in this book.

The illustrate the difference between the two models, consider the following definitions:

<pre>
(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (f a) (sum-of-squares (+ a 1) (* a 2)))
</pre>

Evaluation of the procedure `f` is straightforward enough:

<pre>
(f 5)

==> 136
</pre>

But the question we wish to concern ourselves with is: How (exactly) does the interpreter go about arriving at this answer?

First, consider that the expression `(f 5)` is comprised of two sub-expressions: the operator `f` and the operand `5`. 

Evaluation of the operator `5` simply yields `5`, which is to say, `5` already is a primtive and is not susceptible to further evaluation. 

The operator `f`, however, can be further reduced to its definition in terms of the `sum-of-squares` procedure.

So the first step in our evaluation model yields the following result:

<pre>
(f 5)

(sum-of-squares (+ 5 1) (* 5 10))
</pre>

At which point, the **recursive** nature of expression evaluation becomes evident: the problem of evaluating the expression `(f 5)` reduces to the problem of evaluating the expression `(sum-of-squares (+ 5 1) (* 5 10))`; that is to say, we must again evaluate the operands of the expression (in this case, `(+ 5 1)` and `(* 5 10)`), and then we must apply these operands to the operator `sum-of-squares`.

Proceeding with the evaluation, we have:

<pre>
(f 5)

(sum-of-squares (+ 5 1) (* 5 2))

(+ (square 6) (square 10))
</pre>

Using the definition of the procedure `square`, we get:

<pre>
(+ (* 6 6) (* 10 10))

(+ 36 100)

136
</pre>

Normal-Order Evaluation
-----------------------

The evaluation model described above is not the only way to perform expression evaluation. 

An alternative evaluation model would **not** evaluate the operands until their values were actually needed. Instead, it would first substitute operand expressions for parameters until it obtained an expression involving only primitive operators, and then it would perform the evaluation.

Such a model of expression evaluation is called normal-order evaluation.

In the normal-order model of expression evaluation, evaluation of `(f 5)` would proceed according to the following sequence of operations:

<pre>
(f 5)
 
(sum-of-squares (+ 5 1) (* 5 2))

(+ (square (+ 5 1)) (square (* 5 2)))

(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
</pre>

Having now arrived at an expression involving only primitive operations, the evaluator would proceed to evaluate the operands:

<pre>
(+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))

(+ (* 6 6) (* 10 10))

(+ 36 100)

136
</pre>

It can be shown that for procedures that can be modeled using substitution and that yield legitimate values, normal-order and applicative-order evaluation produce the same value. 

Lisp uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluations of expressions such `(+ 5 1)` and `(* 5 2)` above, and more significantly, because normal-order evaluation becomes much more complicated to deal with when we leave the realm of procedures that can be modeled by substitution.
