Exercise 1.23
============= 

The `smallest-divisor` procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2, there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for `test-divisor` should not be 2, 3, 4, 5, 6, ... but rather 2, 3, 5, 7, 9, ... To implement this change, define a procedure `next` that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the `smallest-divisor` procedure to use `(next test-divisor)` instead of `(+ test-divisor 1)`. With `timed-prime-test` incorporating this modified version of `smallest-divisor`, run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

Solution
-------- 

Supporting Scheme procedures are given in the corresponding .scm source file.

The statistics below were collected for the same set of 9 prime numbers that were tested in Exercise 1-22.

Clearly, it appears that the primality testing procedure ran faster in this round of tests. A more precise quantitative analysis of the running time follows the discussion of the statistics.

### One Billion ###

The first three primes larger than 1 billion are 1,000,000,007; 1,000,000,009; and 1,000,000,021.

The following statistics describe how many milliseconds it takes for `timed-prime-test` procedure to determine whether the number in question is prime:

```scheme
1,000,000,007: (58, 58, 58, 58, 58, 58, 58, 59, 83, 58)
1,000,000,009: (57, 57, 81, 58, 58, 58, 57, 57, 58, 82)
1,000,000,021: (58, 59, 58, 58, 57, 81, 58, 57, 58, 58)
```

Averaging these results, we obtain:

```scheme
1,000,000,007 - Mean: 60.6, Standard Deviation: 7.8
1,000,000,009 - Mean: 62.3, Standard Deviation: 10.1
1,000,000,021 - Mean: 60.2, Standard Deviation: 7.3
```

The average running time for `timed-prime-test` to find a prime near 1,000,000,000 is roughly <strong>61.0 (+- 8.3) ms</strong>.

### Ten Billion ###

The first three primes larger than 10 billion are 10,000,000,019; 10,000,000,033; and 10,000,000,061.

The following statistics describe how many milliseconds it takes for `timed-prime-test` procedure to determine whether the number in question is prime:

```scheme
10,000,000,019: (187, 187, 188, 188, 212, 187, 211, 187, 186, 210)
10,000,000,033: (211, 187, 187, 211, 186, 211, 187, 187, 186, 186)
10,000,000,061: (187, 187, 211, 186, 211, 187, 210, 188, 187, 211)
```

Averaging these results, we obtain:

```scheme
10,000,000,019 - Mean: 194.3, Standard Deviation: 11.5
10,000,000,033 - Mean: 193.9, Standard Deviation: 11.8
10,000,000,061 - Mean: 196.5, Standard Deviation: 12.3
```

The average running time for `timed-prime-test` to find a prime near 10,000,000,000 is roughly <strong>194.9 (+- 11.5) ms</strong>.

### One Hundred Billion ###

The first three primes larger than 100 billion are 100,000,000,003; 100,000,000,019; and 100,000,000,057.

The following statistics describe how many milliseconds it takes for `timed-prime-test` procedure to determine whether the number in question is prime:

```scheme
100,000,000,003: (644, 619, 622, 643, 619, 621, 644, 620, 621, 645)
100,000,000,019: (620, 645, 619, 620, 644, 618, 621, 644, 620, 620)
100,000,000,057: (621, 618, 620, 622, 620, 645, 622, 620, 646, 624)
```

Averaging these results, we obtain:

```scheme
100,000,000,003 - Mean: 629.8, Standard Deviation: 12.3
100,000,000,019 - Mean: 627.1, Standard Deviation: 11.9
100,000,000,057 - Mean: 625.8, Standard Deviation: 10.5
```

The average running time for `timed-prime-test` to find a prime near 100,000,000,000 is roughly <strong>627.7 (+- 11.3) ms</strong>.

Analysis
--------

For primes around 1 billion, the average running time was reduced from 97.4 ms to 61.0 ms (improvement factor of <strong>1.60</strong>). 

For primes around 10 billion, the average running time was reduced from 341.6 ms to 194.9 ms (improvement factor of <strong>1.75</strong>).

For primes around 100 billion, the average running time was reduced from 1065.0 ms to 627.8 ms (improvement factor of <strong>1.70</strong>). 

Clearly, these are substantial performance improvements, but they are not quite the <strong>2x</strong> improvement that we were anticipating. 

One possible reason for this is that while the `find-divisor` procedure is invoked only 1/2 as frequently as before, the procedure used to determine the next `test-divisor` is more complex than the one used in Exercise 1-22. Specifically, in Exercise 1-22, we used a simple addition operation to calculate the next `test-divisor`:

```scheme
(+ test-divisor 1)
```

While in Exercise 1-23, the procedure used to calculate the next `test-divisor` involves the evaluation of a conditional:

```scheme
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))
```

The `next` procedure so defined is relatively inefficient: the conditional so evaluated only works out to 2 once, and all other invocations of `next` return the `(+ n 2)` form. 

This suggests modifying our code in such a way that the `next` procedure used by `find-divisor` <strong>always</strong> returns `(+ n 2)`. We accomplish this by changing the definition of `smallest-divisor`, so that if the smallest divisor works out to be 2, we return immediately with no need to invoke `find-divisor`. If the smallest divisor is larger than 2, we go ahead and invoke `find-divisor`, but we do so with the knowledge that all subsequent test divisors can be determined using the `(+ n 2)` form exclusively. 

The modifications to the source code would be rendered as follows:

```scheme
(define (smallest-divisor n)
  (if (divides? 2 n)
      2
      (find-divisor n 3)))
 
(define (next n)
  (+ n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
```

We summarize the performance results of this code change in Section B of this answer.
