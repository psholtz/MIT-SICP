Exercise 1.22
============= 

Most Lisp implementations include a primitive called `runtime` that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following `timed-prime-test` procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

```scheme
(define (timed-prime-test n)
	(newline)	  
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
	    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))
```

Using this procedure, write a procedure `search-for-primes` that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1,000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of O(sqrt(n)), you should expect that testing for primes around 10,000 should take about sqrt(10) times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the sqrt(n) prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?

Solution
-------- 

The supporting Scheme procedures are given in the accompanying .scm source file. 

SICP was written in the 1980s. Since that time, computing power has increased exponentially, to the point where a search for primes of size ~1,000 can be carried out almost trivially. For purposes of profiling and discussion, we will carry out the search for primes of size (a) 1,000,000,000, (b) 10,000,000,000 and (c) 100,000,000,000 respectively.

We define the respective "starting points" for the primality search, and find the first three primes beyond each "starting point":

```scheme
(define point1 1000000000)
(define point2 (* 10 point1))
(define point3 (* 10 point2))

(search-for-n-primes point1 3)
(search-for-n-primes point2 3)
(search-for-n-primes point3 3)
```

### One Billion ###

The first three primes larger than 1 billion are 1,000,000,007; 1,000,000,009; and 1,000,000,021. 

Running the `(search-for-n-primes point1 3)` procedure 10 times, we collect the following statistics for how many milliseconds it takes for the `timed-prime-test` procedure to determine whether the number in question is prime or not:

```scheme
1,000,000,007: (95, 94, 119, 94, 95, 119, 96, 95, 120, 95)
1,000,000,009: (95, 95, 95, 95, 95, 95, 95, 95, 95, 95)
1,000,000,021: (95, 94, 95, 95, 96, 95, 95, 95, 95, 95)
```

Averaging these results, we obtain:

```scheme
1,000,000,007 - Mean: 102.2, Standard Deviation: 11.8
1,000,000,009 - Mean: 95.0, Standard Deviation: 0.00
1,000,000,021 - Mean: 95.0, Standard Deviation: 0.47
```

We surmise that the average running time for `timed-prime-test` to find a prime near 1,000,000,000 is roughly <strong>97.4 (+- 7.5) ms</strong>

### Ten Billion ###

The first three primes larger than 10 billion are 10,000,000,019; 10,000,000,033; and 10,000,000,061. 

Running the `(search-for-n-primes point2 3)` procedure 10 times, we collect the following statistics for how many milliseconds it takes for the `timed-prime-test` procedure to determine whether the number in question is prime or not:

```scheme
10,000,000,019: (336, 335, 334, 336, 308, 332, 309, 334, 312, 333)
10,000,000,033: (315, 336, 311, 337, 336, 335, 335, 335, 437, 336)
10,000,000,061: (340, 311, 336, 315, 337, 312, 335, 311, 636, 334) 
```

Averaging these results, we obtain:

```scheme
10,000,000,019 - Mean: 326.9, Standard Deviation: 12.0
10,000,000,033 - Mean: 341.3, Standard Deviation: 34.9
10,000,000,061 - Mean: 356.7, Standard Deviation: 98.9
```

We surmise that the average running time for `timed-prime-test` to find a prime near 10,000,000,000 is roughly <strong>341.6 (+- 60.1) ms</strong>.

### One Hundred Billion ###

The first three primes larger than 100 billion are 100,000,000,003; 100,000,000,019; and 100,000,000,057.

Running the `(search-for-n-primes point3 3)` procedure 10 times, we collect the following statistics for how many milliseconds it takes for the `timed-prime-test` procedure to determine whether the number in question is prime or not:

```scheme
100,000,000,003: (1048, 1070, 1052, 1066, 1068, 1047, 1098, 1051, 1068, 1055)
100,000,000,019: (1098, 1046, 1070, 1050, 1048, 1082, 1047, 1078, 1051, 1049)
100,000,000,057: (1075, 1050, 1049, 1052, 1092, 1048, 1169, 1050, 1074, 1048)
```

Averaging these results, we obtain:

```scheme
100,000,000,003 - Mean: 1062.3, Standard Deviation: 15.4
100,000,000,019 - Mean: 1062.0, Standard Deviation: 18.6
100,000,000,057 - Mean: 1070.7, Standard Deviation: 35.8
```

We surmise that the average running time for `timed-prime-test` to find a prime near 100,000,000,000 is roughly <strong>1065.0 (+- 25.3) ms</strong>.

Analysis
-------- 

As described in the text, the order of growth for this algorithm is O(sqrt(n)), and so between the three data sets given above, we would expect the running time to slow down by a factor of sqrt(10) ~ 3.16 as the size of the primes we are search for increases by a factor of 10. 

### One Billion ###

Running time for 1,000,000,000: <strong>97.4 (+- 7.5) ms</strong> 

Expected running time for 10,000,000,000: <strong>308.0 ms</strong> -- 
Actual running time for 10,000,000,000: <strong>341.6 (+- 60.1) ms</strong>

Expected running time for 100,000,000,000: <strong>974.0 ms</strong> --
Actual running time for 100,000,000,000: <strong>1065.0 (+- 25.3) ms</strong>

### Ten Billion ###

Running time for 10,000,000,000: <strong>341.6 (+- 60.1) ms</strong>

Expected running time for 1,000,000,000: <strong>108.0</strong> --
Actual running time for 1,000,000,000: <strong>97.4 (+- 7.5 ms</strong>

Expected running time for 100,000,000,000: <strong>1080.2</strong> --
Actual running time for 100,000,000,000: <strong>1065.0 (+- 25.3) ms</strong>

### One Hundred Billion ###

Running time for 100,000,000,000: <strong>1065.0 (+- 25.3) ms</strong>

Expected running time for 1,000,000,000: <strong>106.5</strong> --
Actual running time for 1,000,000,000: <strong>97.4 (+- 7.5) ms</strong>

Expected running time for 10,000,000,000: <strong>336.8</strong> --
Actual running time for 10,000,000,000: <strong>341.6 (+- 60.1) ms</strong>

The results obtained are generally within the bounds of experimental error, and do seem to trend at the expected rate of sqrt(10).
