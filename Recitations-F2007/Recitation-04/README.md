Recitation 4 - Sept 14, 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r04.pdf))
================================== 

Orders of Growth
---------------- 

xx

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