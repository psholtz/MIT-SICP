;;
;; As with the emacs version, we can define these procedures for the fun
;; of doing so, but Clojure cannot compute primes large enough to generate
;; interesting statistics (the Clojure model generates false negatives)
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
    (+ 2 (random (- n 4))))
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