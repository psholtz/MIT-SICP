Exercise 1.23b
============== 

The optimization made in Exercise 1.23 resulted in a substantial increase in running time (~1.6x to ~1.8x), but was not quite the 2x performance increase that we had anticipated.

Let's see if we can make even further optimizations and come closer to the 2x benchmark. 

Specifically, in Exercise 1.23, the procedure used to calculate the next `test-divisor` involved evaluation of a conditional:

```scheme
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))
```

The `next` procedure so defined is relatively inefficient: the conditional so evaluated only works out to 2 once, and all other invocations of `next` return the `(+ n 2)` form.

This suggests modifying our code in such a way that the `next` procedure used by `find-divisor` <strong>always</strong> returns `(+ n 2)`. We accomplish this by changing the definition of `smallest-divisor` so that if the smallest divisor works out to be 2, we return immediately with no need to invoke `find-divisor`. If the smallest divisor is larger than 2, we go ahead and invoke `find-divisor`, but we do so with the knowledge that all subsequent test divisors can be determined using the `(+ n 2)` form exclusively.

The modifications to the source code are rendered as follows:

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

Solution 
-------- 

### One Billion ###

The data samples collected for the first three primes beyond 1 billion are:

```scheme
1,000,000,007: (52, 51, 51, 51, 51, 51, 51, 51, 51, 51)
1,000,000,009: (75, 52, 51, 51, 51, 50, 51, 75, 51, 50)
1,000,000,021: (51, 53, 75, 52, 51, 51, 51, 52, 52, 51)
```

Averaging these results, we obtain:

```scheme
1,000,000,007 - Mean: 51.1, Standard Deviation: 0.3
1,000,000,009 - Mean: 55.7, Standard Deviation: 10.2
1,000,000,021 - Mean: 53.9, Standard Deviation: 7.4
```

The average running time works out to <strong>53.6 (+- 7.3) milliseconds.</strong>

### Ten Billion ###

The data samples collected for the first three primes beyond 10 billion are:

```scheme
10,000,000,019: (189, 165, 169, 191, 167, 190, 166, 191, 166, 168)
10,000,000,033: (166, 191, 166, 167, 191, 166, 190, 167, 193, 168)
10,000,000,061: (192, 166, 167, 191, 167, 192, 168, 191, 166, 167)
```

Averaging these results, we obtain:

```scheme
10,000,000,019 - Mean: 176.2, Standard Deviation: 12.2
10,000,000,033 - Mean: 176.5, Standard Deviation: 12.7
10,000,000,061 - Mean: 175.5, Standard Deviation: 11.9
```

The average running time works out to <strong>176.1 (+- 11.8) milliseconds.</strong>

### One Hundred Billion ###

The data samples collected for the first three primes beyond 100 billion are:

```scheme
100,000,000,003: (560, 604, 556, 558, 579, 559, 558, 581, 559, 559)
100,000,000,019: (559, 555, 583, 558, 558, 578, 555, 558, 580, 558)
100,000,000,057: (559, 580, 557, 556, 578, 554, 557, 584, 646, 559)
```

Averaging these results, we obtain:

```scheme
100,000,000,003 - Mean: 567.3, Standard Deviation: 15.7
100,000,000,019 - Mean: 564.2, Standard Deviation: 11.3
100,000,000,057 - Mean: 573.0, Standard Deviation: 28.0
```

The average running time works out to <strong>568.2 (+- 1.3) milliseconds.</strong>

Analysis
-------- 

This code optimization gives a performance improvement closer to the targeted 2x factor:

<ul>
	<li>For primes around 1 billion, the average running time was reduced from 97.4 ms to 61.0 ms to 53.6 ms.</li>

	<li>For primes around 10 billion, the average running time was reduced from 341.6 ms to 194.9 ms to 176.1 ms.</li>

	<li>For primes around 100 billion, the average running time was reduced from 1065.0 ms to 627.8 ms to 568.2 ms.</li>
</ul>

For primes around 1 billion, this results in a performance improvement of roughly 1.82x and 1.14x respectively. For primes around 10 billion, this results in a performance improvement of roughly 1.94x and 1.11x respectively. For primes around 100 billion, this results in a performance improvement of roughly 1.87x and 1.10x respectively.

This second optimization gives a performance improvement closer to the 2x factor that was originally anticipated.
