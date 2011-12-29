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

The period of an LCD is at most m. If the other two constants are not chosen correctly, its period may be much shorter than that. The attributes that the variables must meet to have full cycles is:

1. b and m are relatively prime.
2. a-1 is divisible by all prime factors of m.
3. a-1 is a multiple of 4 if m is a multiple of 4.

A good choice of variables would be:

<pre>
m = 2^32
a = 1664525
b = 1013904423
</pre>

A more thorough discussion of LCGs can be had on Wikipedia: http://en.wikipedia.org/wiki/Linear_congruential_generator