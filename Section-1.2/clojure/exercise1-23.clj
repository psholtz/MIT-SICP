;;
;; Exercise 1.23
;;
;; The "smallest-divisor" procedure shown at the start of this section does lots of
;; needless testing: After it checks to see if the number is divisible by 2, there
;; is no point in checking to see if it is divisible by any larger even numbers. This
;; suggests that the values used for "test-divisor" should not be 2,3,4,5,6,... but
;; rather 2,3,5,7,9... To implement this change, define a procedure "next" that returns
;; 3 if its input is equal to 2, and otherwise returns its input plus 2. Modify the
;; "smallest-divisor" procedure to use "(next test-divisor)" instead of "(+ test-divisor 1)".
;; With "timed-prime-test" incorporaitng this modified version of "smallest-divisor", run
;; the test for each of the 12 primes found in exercise 1.22. Since this modification
;; halves the number of test steps, you should expect it to run about twice as fast.
;; Is this expectation confirmed? If not, what is the observed ratio of the speeds of
;; the two algorithms, and how do you explain the fact that it is different from 2?
;;

;;
;; As before, let's first define the code that allows us to check for primes.
;;
;; The major difference between this code and that in Exercise 1.22 is
;; that here we have defined a new procedure "next", which should cut
;; down the time needed to test a number for primality roughly by half.
;;
;; The statistics gathered at the end of this document, and discussed
;; in greater depth in the corresponding .md file, give a more detailed
;; analysis of the performance improvement resulting from this code modification.
;;
(defn square [x] (* x x))

(defn divides? [a b]
  (= (rem b a) 0))

;; "next" is already a built-in procedure in clojure
(defn next-divisor [n]
  (cond (= n 2) 3
        :else (+ n 2)))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (next-divisor test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

;;
;; Next, define the procedures for running timed tests.
;;
;; We'll change this somewhat from the procedures presented in the text,
;; in that our procedure will only print a number (and corresponding time)
;; if it's prime.
;;

;;
;; Modify procedure slightly, from what is defined in the text, so that
;; we only print the prime numbers (i.e., non-primes are suppressed).
;;
(defn report-prime [n elapsed-time]
  (println)
  (print n)
  (print " (")
  (print elapsed-time)
  (print ")"))

;;
;; Use this definition of start-prime-test, which returns "true" or "false"
;; depending on whether the test candidate is prime, so that we can more easily
;; support the "search-for-n-primes" procedure defined below.
;;
(defn start-prime-test [n start-time]
  (cond (prime? n)
        (do
          (report-prime n (- (System/currentTimeMillis) start-time))
          true)
        :else false))

;;
;; In Clojure, we can make use of the Java libraries for system time.
;;
(defn timed-prime-test [n]
  (start-prime-test n (System/currentTimeMillis)))

;;
;; Finally, let's define the "search-for-primes" procedure.
;;
;; The procedure will take two integers, a and b, and for each prime
;; inbetween the two integers (inclusive) it will print the prime out
;; and display the time required to calculate that it was a prime.
;;
(defn search-for-primes [a b]
  (defn search [n]
    (cond (<= n b) (timed-prime-test n))
    (cond (< n b) (search (+ n 2))))
  (if (even? a)
    (search (+ a 1))
    (search a)))

(search-for-primes 10000 10050)
;; ==> 10007 (1)
;; ==> 10009 (0)
;; ==> 10037 (0)
;; ==> 10039 (0)

(search-for-primes 100000 100050)
;; ==> 100003 (2)
;; ==> 100019 (2)
;; ==> 100043 (1)
;; ==> 100049 (1)

(search-for-primes 1000000 1000050)
;; ==> 1000003 (2)
;; ==> 1000033 (2)
;; ==> 1000037 (2)
;; ==> 1000039 (1)

;;
;; Now define one additional procedure, which starts at a number a
;; and finds the next n prime numbers (this is, technically, what
;; Exercise 1.22 asks us to do).
;;
(defn search-for-n-primes [a n]
  (defn search [j c]
    (let [next-j (+ j 2)]
      (cond (< c n)
            (if (timed-prime-test j)
              (search next-j (+ c 1))
              (search next-j c)))))
  (if (even? a)
    (search (+ a 1) 0)
    (search a 0)))

(search-for-n-primes 10000 3)
;; ==> 10007 (0)
;; ==> 10009 (1)
;; ==> 10037 (1)

(search-for-n-primes 100000 3)
;; ==> 100003 (0)
;; ==> 100019 (1)
;; ==> 100043 (1)

(search-for-n-primes 1000000 3)
;; ==> 1000003 (1)
;; ==> 1000033 (1)
;; ==> 1000037 (1)

;;
;; These results do seem "faster" than those obtained in the previous version of prime?
;; 