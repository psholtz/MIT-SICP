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