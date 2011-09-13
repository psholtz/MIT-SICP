;;
;; Exercise 1.22
;;
;; Most Lisp implementations include a primitive called "runtime" that returns an
;; integer that specifies the amount of time the system has been running (measured,
;; for example, in microseconds). The following "timed-prime-test" procedure, when
;; called with an integer n, prints n and checks to see if n is prime. If n is
;; prime, the procedure prints three asterisks follows by the amount of time used
;; in performing this test.
;;
;; Using this procedure, write a procedure "search-for-primes" that checks the
;; primality of consecutive odd integers in a specified range. Use your procedure
;; to find the smallest primes larger than 1000; larger than 10,000; larger than 100,000;
;; larger than 1,000,000. Note the time needed to test each prime. Since the testing
;; algorithm has order of growth O(sqrt(n)), you should expect that testing for primes
;; around 10,000 should take about sqrt(10) times as long as testing for primes around
;; 1,000. Do your timing data bear this out? How well do the data for 100,000 and
;; 1,000,000 support the sqrt(n) prediction? Is your result compatible with the notion
;; that programs on your machine run in time proportional to the number of steps
;; required for the computation.
;;

;;
;; First let's define the code that allows us to check for primes:
;;
(defn smallest-divisor [n]
  (find-divisor n 2))

(defn find-divisor [n test-divisor]
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        :else (find-divisor n (+ test-divisor 1))))
                 
(defn divides? [a b]
  (= (mod b a) 0))

(defn square [n] (* n n))

(defn prime? [n]
  (= n (smallest-divisor n)))
