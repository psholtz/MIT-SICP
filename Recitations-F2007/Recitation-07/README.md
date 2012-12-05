Recitation 7 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r07.pdf))
=======================================================================================

List Functions
-------------- 

<pre>
(define (length lst)
 (if (null? lst)
  0
  (+ 1 (length (cdr lst)))))


</pre>