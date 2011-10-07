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
    (print "Statistics for the prime ")
    (println number)
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