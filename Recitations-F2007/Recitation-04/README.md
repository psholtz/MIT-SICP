Recitation 4 - Sept 14, 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r04.pdf))
================================== 

Orders of Growth
---------------- 

Ignore constants. Ignore lower order terms. For a sum, take the larger term. For a product, multiply the two terms. Orders of growth are concerned with how the effort scales up as the size of the problem increases, rather than an exact measure of the cost.

Typical Orders of Growth
------------------------ 

O(1) - Constant growth. Simple, non-looping, non-decomposible operations have constant growth.
O(log n) - Logarithmic growth. At each iteration, the problem size is scaled down by a constant amount: (call-again (/ n c)).
O(n) - Linear growth. At each iteration, the problem size is decremented by a constant amount: (call-again (- n c)).
O(n log n) - Nifty growth. Nice recursive solution to normally O(n^2) problem.
O(n^2) - Quadratic growth. Computing correspondence between a set of n things, or doing something of cost n to all n things both result in quadratic growth.
O(2^n) - Exponential growth. Really bad. Searching all possibilities usually results in exponential growth.

Exercises
--------- 

(1) Give order notation for the following:

 (a) 5n^2 + n

 (b) sqrt(n) + n

 (c) 3^n * n^2

 (d) log(57) n + 1/n^2

(2) What is the order of growth of the following procedure to calculate b^n by repeated multiplication?

<pre>
(define (expt b n)
 (if (= n 0)
     1
     (* b (expt b (- n 1)))))
</pre>

Running time? Space?

(2) What is the order of growth of the following procedure to calculate b^n by repeated multiplication?

<pre>
(define (expt b n)
 (if (= n 0)
     1
     (* b (expt b (- n 1)))))
</pre>

Running time? Space? 

(3) Rewrite the previous definition so that it yields an iterative process. Running time? Space?

(4) Now write a version of "expt" that takes less than O(n) time. Running time? Space?

(5) One last "expt" version: this time, both time and space must be less than O(n). Running time? Space?

(6) Consider the recursive definition of factorial we've seen before:

<pre>
(define (fact n)
 (if (= n 0)
     1 
     (* n (fact (- n 1)))))
</pre>

Running time? Space?

(7) Assume you have a procedure (divisible? n x) which returns #t if n is divisible by x. It runs in 
O(n) time and O(1) space. Write a procedure prime? which takes a number and returns #t if it's prime and 
\#f otherwise. You'll want to use a helper procedure.

Running time? Space?

(8) Write a procedure that will multiply two positive integers together, but the only arithmetic operation 
allowed is addition (i.e., multiplication through repeated addition). In addition, your procedure should
be iterative, not recursive. What is its order of growth?

(slow-mul 3 4) -> 12