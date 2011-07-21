Exercise 1.24
============= 

Modify the `timed-prime-test` procedure of Exercise 1.22 to use `fast-prime?` (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has O(lg n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepency you find?

Solution
-------- 

SICP was written in the mid-1980s. For a circa 2010 computing device, the Fermat Method simply executes too fast for primes near 1 million (or, for that matter, even near 100 billion) to be "measurable" .. that is, the method returns nearly "instantaneously". The data collected in the accompanying .scm file for primes near 100 billion all are in the range of 1 to 2 millseconds, too small to determine a meaningful statistical distribution. 

Accordingly, we will "start" the evaluation at a google, that is, at 10^100. 

We will collect statistics for finding primes near a google, and for finding primes near a google squared, and see whether these results match our expectations.

The following statistics were collected for primes near a google:

<pre>
(define p1 (+ google 267))
(define p2 (+ google 949))
(define p3 (+ google 1243))

(statistics p1 10)
;; --> (30 30 30 57 31 30 31 32 30 30)
;; --> Mean: 33.1 milliseconds
;; --> Std Dev: 8.43

(statistics p2 10)
;; --> (31 55 33 31 31 30 33 30 30 31)
;; --> Mean: 33.5 milliseonds
;; --> Std Dev: 7.64 

(statistics p3 10)
;; --> (30 30 31 33 31 31 31 33 30 55)
;; --> Mean: 33.5 milliseconds 
;; --> Std Dev: 7.64
</pre>

The average time for primality testing for primes near a google is <strong>33.4 (+- 7.6) milliseconds</strong>

The following statistics were collected for primes near a google-squared:

<pre>
(define q1 (+ google 357))
(define q2 (+ google 627))
(define q3 (+ google 799))

(statistics q1 10)
;; --> (140 165 141 141 165 140 141 165 141 140)
;; --> Mean: 147.9 milliseconds
;; --> Std Dev: 11.8

(statistics q2 10)
;; --> (140 140 164 142 141 166 140 166 140 140)
;; --> Mean: 147.9 milliseconds
;; --> Std Dev: 12.1

(statistics q3 10)
;; --> (141 141 165 140 140 166 141 141 166 141)
;; --> Mean: 148.2 milliseconds
;; --> Std Dev: 12.1
</pre>

The average time for primality testing for primes near a google-squared is <strong>148.0 (+- 11.6) milliseconds</strong>.

The order of growth for this procedure is O(lg n). We would therefore expect the running time for a prime of size n^2 to run (roughly) twice as slowly as that for a prime of size n: O(lg n^2) = O(2 * lg n) = 2 * O(lg n). In this case, however, the procedure seems to run about 4x more slowly for primes near a google-squared, as compared with primes near a google. The reason for this likely has to do with other numerical and/or computational factors involved with handling such large numbers.