Section 3.1
=========== 

Assignment and Local State
--------------------------

[xx]

Random Variables
---------------- 

The text defines the following code:

<pre>
(define rand
  (let ((x random-init))
    (lambda (x)
     (set! x (rand-update x))
      x)))
</pre>

However, they do not define the "rand-update" procedure. Instead, they only mention that a linear congruential generator can be used to generate the requisite random sequence. A LCG works as a decent PRNG by the following simple recurrence relation:

<pre>
x(n+1) = (a*x(n) + b) mod m
</pre>

where a, b and m are appropriately chosen. 

A more thorough discussion of LCGs can be had on Wikipedia: http://en.wikipedia.org/wiki/Linear_congruential_generator