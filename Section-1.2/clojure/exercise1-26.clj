;;
;; Exercise 1.26
;;
;; Louis Reasoner is having great difficulty doing exercise 1.24. This "fast-prime?" test seems to
;; run more slowly than this "prime?" test. Louis calls his friend Eva Lu Ator over to help. When
;; they examine Louis' code, they find that he has rewritten the expmod procedure to use an explicit
;; multiplication, rather than calling "square":
;;
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                      m))
;;          (else
;;           (remainder (* base (expmod base (- exp 1) m))
;;                      m))))
;;
;; "I don't see what difference that could make," says Louis. "I do." says Eva. "By writing the
;; "procedure like that you have transformed the O(log n) process into a O(n) process." Explain.
;;

;;
;; For the sake of entertainment, let's set ourselves up to run the prime number tests using the
;; version of "expmod" given above:
;;
(defn square [x] (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (* (expmod base (/ exp 2) m)
                            (expmod base (/ exp 2) m)) m)
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
(def n 100) ;; let's run the test 100 times

(fast-prime? 3 n)
;; --> true

(fast-prime? 4 n)
;; --> false

(fast-prime? 5 n)
;; --> true

(fast-prime? 6 n)
;; --> false

(fast-prime? 7 n)
;; --> true

;;
;; Next, define the procedures for running timed tests.
;;
;; We'll change this somewhat from the procedures presented in the text
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
;; ==> 10007 (53)
;; ==> 10009 (36)
;; ==> 10037 (26)
;; ==> 10039 (21)

(search-for-primes 100000 100050)
;; ==> 100003 (194)
;; ==> 100019 (257)
;; ==> 100043 (189)
;; ==> 100049 (193)

(search-for-primes 1000000 1000050)
;; ==> 1000003 (1769)
;; ==> 1000033 (1759)
;; ==> 1000037 (1863)
;; ==> 1000039 (1993)

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
;; ==> 10007 (21)
;; ==> 10009 (24)
;; ==> 10037 (23)

(search-for-n-primes 100000 3)
;; ==> 100003 (192)
;; ==> 100019 (221)
;; ==> 100043 (323)

(search-for-n-primes 1000000 3)
;; ==> 1000003 (2345)
;; ==> 1000033 (1925)
;; ==> 1000037 (1916)

;;
;; Clearly the performance of this procedure is **much** slower than that given in
;; Exercise 1-22, and just as clearly the reason is because we are performing the
;; recursive calculation "expmod" twice, rather than just once as in Exercise 1-22.
;;
;; Let's expand the expression (expmod 5 5 3) using both models, to get a better sense
;; of what's going on.
;;
;; First we expand using the "slow" computational model:
;;
(expmod 5 5 3)
(rem (* 5 (expmod 5 4 3)) 3)
(rem (* 5 (rem (* (expmod 5 2 3) (expmod 5 2 3)) 3)) 3)
(rem (* 5 (rem (* (rem (* (expmod 5 1 3) (expmod 5 1 3)) 3)
                  (rem (* (expmod 5 1 3) (expmod 5 1 3)) 3)) 3)) 3)
(rem (* 5 (rem (* (rem (* (rem (* 5 (expmod 5 0 3)) 3) (rem (* 5 (expmod 5 0 3)) 3)) 3)
                  (rem (* (rem (* 5 (expmod 5 0 3)) 3) (rem (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(rem (* 5 (rem (* (rem (* (rem (* 5 1) 3) (rem (* 5 1) 3)) 3)
                  (rem (* (rem (* 5 1) 3) (rem (* 5 1) 3)) 3)) 3)) 3)
(rem (* 5 (rem (* (rem (* (rem 5 3) (rem 5 3)) 3)
                  (rem (* (rem 5 3) (rem 5 3)) 3)) 3)) 3)
(rem (* 5 (rem (* (rem (* 2 2) 3)
                  (rem (* 2 2) 3)) 3)) 3)
(rem (* 5 (rem (* (rem 4 3) (rem 4 3)) 3)) 3)
(rem (* 5 (rem (* 1 1) 3)) 3)
(rem (* 5 (rem 1 3)) 3)
(rem (* 5 1) 3)
(rem 5 3)
2

;;
;; In this computation, the "expmod" procedure is invoked 12 times.
;;

;;
;; Now let's expand this expression using the "fast" computational model:
;;
(expmod 5 5 3)
(rem (* 5 (expmod 5 4 3)) 3)
(rem (* 5 (rem (square (expmod 5 2 3)) 3)) 3)
(rem (* 5 (rem (square (rem (square (expmod 5 1 3)) 3)) 3)) 3)
(rem (* 5 (rem (square (rem (square (rem (* 5 (expmod 5 0 3)) 3)) 3)) 3)) 3)
(rem (* 5 (rem (square (rem (square (rem (* 5 1) 3)) 3)) 3)) 3)
(rem (* 5 (rem (square (rem (square (rem 5 3)) 3)) 3)) 3)
(rem (* 5 (rem (square (rem (square 2) 3)) 3)) 3)
(rem (* 5 (rem (square (rem 4 3)) 3)) 3)
(rem (* 5 (rem (square 1) 3)) 3)
(rem (* 5 (rem 1 3)) 3)
(rem (* 5 1) 3)
(rem 5 3)
2
 
;;
;; In this computaiton, the "expmod" procedure is invoked 5 times.
;;
;; Clearly this second procedure will execute much more rapidly.
;;