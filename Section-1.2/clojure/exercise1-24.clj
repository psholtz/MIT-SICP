;;
;; Exercise 1.24
;;
;; Modify the "timed-prime-test" procedure of Exercise 1.22 to use "fast-prime?" (the Fermat method), and test
;; each of the 12 primes you found in that exercise. Since the Fermat test has O(lg n) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1,000? Do
;; your data bear this out? Can you explain any discrepency you find?
;;

;;
;; First let's define the code that allows us to check for primes:
;;
(defn square [x] (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
        :else (rem (* base (expmod base (- exp 1) m)) m)))

(defn random [n]
  (Math/floor (rand n)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

;;
;; Let's run some unit tests, to make sure it works:
;;
(fermat-test 3)
;; --> true

(fermat-test 4)
;; --> false

(fermat-test 5)
;; --> true

(fermat-test 6)
;; --> false

(fermat-test 7)
;; --> true

(def n 100)
(fast-prime? 3 n)
;; ==> true
(fast-prime? 4 n)
;; ==> false
(fast-prime? 5 n)
;; ==> true
(fast-prime? 6 n)
;; ==> false
(fast-prime? 7 n)
;; ==> true

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
  (def times-to-run-test 10)
  (cond (fast-prime? n times-to-run-test)
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
;; ==> 10009 (1)
;; ==> 10037 (1)
;; ==> 10039 (1)

(search-for-primes 100000 100050)
;; ==> 100003 (0)
;; ==> 100019 (0)
;; ==> 100043 (0)
;; ==> 100049 (1)

(search-for-primes 1000000 1000050)
;; ==> 1000003 (1)
;; ==> 1000033 (1)
;; ==> 1000037 (0)
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
;; ==> 10009 (0)
;; ==> 10037 (1)

(search-for-n-primes 100000 3)
;; ==>100003 (0)
;; ==> 100019 (0)
;; ==> 100043 (1)

(search-for-n-primes 1000000 3)
;; ==> 100003 (0)
;; ==> 100019 (0)
;; ==> 100043 (1)

;;
;; These results seem very substantially faster than the results obtained in the 1.22 version of prime?
;;