;;
;; Statistics
;;
;; Collect some performance statistics, to see how this procedure compares with the other prime
;; testing procedures we developed in this section.
;;
(defn square [n] (* n n))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
        :else (rem (* base (expmod base (- exp 1) m)) m)))

;;
;; Define the prime-testing procedure
;;
(defn random [n]
  (Math/floor (rand n)))

(defn prime? [n]
  (defn get-random-a []
    (long (+ 2 (random (- n 4)))))
  (defn test-value [a]
    (= (expmod a (- n 1) n) 1))
  (cond (= n 2) true
        (= n 3) true
        (= n 4) false
        (= n 5) true
        :else (and
               (test-value (- n 1))
               (test-value (- n 2))
               (test-value (get-random-a))
               (test-value (get-random-a))
               (test-value (get-random-a)))))

;; avg : list -> float
;; average value for a list of members
(defn avg [a]
  (defn avg-iter [b v i]
    (cond (> (count b) 0) (avg-iter (rest b) (+ (first b) v) (+ i 1))
          :else (/ v (count a))))
  (avg-iter a 0.0 0.0))

;; std : list -> float
;; standard deviation for a list of numbers
(defn std [d]
  (let [a (avg d)]
    (defn std-iter [b v]
      (if (> (count b) 0)
        (std-iter (rest b) (+ (square (- (first b) a)) v))
        (Math/sqrt (/ v (- (count d) 1)))))
    (if (= (count d) 1)
      0.0
      (std-iter d 0.0))))

;; gather-statistics : integer -> float
;; determine number of milliseconds to test the primarlity of n
(defn gather-statistics [number]
  (defn run [start-time]
    (cond (prime? number) (- (System/currentTimeMillis) start-time)
          :else -1))
  (run (System/currentTimeMillis)))

(defn gather-n-statistics [number times console]
  (defn gather-n-statistics-iter [i a]
    (let [value (gather-statistics number)]
      (if (> value -1)
        (cond (< i times) (do
                            (cond console (do
                                            (print value)
                                            (if (< i (- times 1))
                                              (print " "))))
                            (gather-n-statistics-iter (+ i 1) (cons value a)))
              :else a)
        false)))
  (gather-n-statistics-iter 0 '()))

;; statistics : integer, integer -> void
;; Largely "ui" oriented method. "number" indicates
;; the number to test for primality, while "times"
;; indicates the number of samples to take in the sample set.

(defn statistics [number times]
  (defn display-statistics []
    (println)
    (println "Statistics for the prime ")
    ;;(println number) ;; dont display number, routine runs faster
    (print "(")
    (def value (gather-n-statistics number times true))
    (println ")")
    (print "Mean: ")
    (def a (avg value))
    (print a)
    (println " milliseconds")
    (def s (std value))
    (print "Std Dev: ")
    (println s))
  (defn display-error []
    (println)
    (print number)
    (println " is not a prime"))
  (cond (prime? number) (display-statistics)
        :else (display-error)))


;;
;; Define the fast-expt procedure, so we can speak in terms of "google"
;;
(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(def google (fast-expt 10 100))
(def google-squared (square google))

(def billion (fast-expt 10 9))
(def ten-billion (* 10 billion))
(def hundred-billion (* 100 billion))

;;
;; Let's collect some statistics for primes near 1 billion:
;;
(statistics (+ billion 7) 10)
;; ==> (1 1 1 1 0 1 1 1 1 1)
;; ==> Mean: 0.9 milliseconds
;; ==> Std Dev: 0.31622776601683794

(statistics (+ billion 9) 10)
;; ==> (0 0 0 1 0 2 0 1 3 0)
;; ==> Mean: 0.7 milliseconds
;; ==> Std Dev: 1.0593499054713802

(statistics (+ billion 21) 10)
;; ==> (1 0 0 0 0 0 0 0 0 0)
;; ==> Mean: 0.1 milliseconds
;; ==> Std Dev: 0.31622776601683794

;;
;; Collect statistics for primes around 10 billion:
;;
(statistics (+ ten-billion 19) 10)
;; ==> (0 0 1 0 1 0 1 1 0 1)
;; ==> Mean: 0.5 milliseconds
;; ==> Std Dev: 0.5270462766947299

(statistics (+ ten-billion 33) 10)
;; ==> (1 0 1 0 1 0 1 0 0 1)
;; ==> Mean: 0.5 milliseconds
;; ==> Std Dev: 0.5270462766947299

(statistics (+ ten-billion 61) 10)
;; ==> (0 0 1 0 0 1 0 0 0 1)
;; ==> Mean: 0.3 milliseconds
;; ==> Std Dev: 0.4830458915396479

;;
;; Collect statistics for primes around 100 billion:
;;
(statistics (+ hundred-billion 3) 10)
;; ==> (0 0 0 0 0 0 0 0 1 0)
;; ==> Mean: 0.1 milliseconds
;; ==> Std Dev: 0.31622776601683794

(statistics (+ hundred-billion 19) 10)
;; ==> (1 0 3 1 0 0 1 0 0 0)
;; ==> Mean: 0.6 milliseconds
;; ==> Std Dev: 0.9660917830792959

(statistics (+ hundred-billion 57) 10)
;; ==> (0 0 1 0 0 1 0 0 1 0)
;; ==> Mean: 0.3 milliseconds
;; ==>Std Dev: 0.4830458915396479

;;
;; Collect statistics for primes around a google:
;;
(statistics (+ google 267) 10)
;; ==> (26 5 6 6 28 11 7 6 6 7)
;; ==> Mean: 10.8 milliseconds
;; ==> Std Dev: 8.702490065109718

(statistics (+ google 949) 10)
;; ==> (46 5 6 6 10 5 5 5 6 7)
;; ==> Mean: 10.1 milliseconds
;; ==> Std Dev: 12.705641791477253

(statistics (+ google 1243) 10)
;; ==> (6 17 6 6 5 5 5 7 6 5)
;; ==> Mean: 6.8 milliseconds
;; ==>Std Dev: 3.6453928305312844

;;
;; Collect statistics for primes around a google-squared:
;;
(statistics (+ google-squared 357) 10)
;; ==> (34 24 21 23 22 21 24 22 24 22)
;; ==> Mean: 23.7 milliseconds
;; ==> Std Dev: 3.802046232695699

(statistics (+ google-squared 627) 10)
;; ==> (24 32 21 23 30 22 22 24 22 23)
;; ==> Mean: 24.3 milliseconds
;; ==> Std Dev: 3.683295625749672

(statistics (+ google-squared 799) 10)
;; ==> (23 23 24 22 23 22 26 23 23 22)
;; ==> Mean: 23.1 milliseconds
;; ==> Std Dev: 1.1972189997378646

;;
;; These are GOOD performance numbers.. better (substantially) than the MIT-Scheme.
;;