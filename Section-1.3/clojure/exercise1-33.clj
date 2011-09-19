;;
;; Exercise 1.33
;;
;; You can obtain an even more general version of "accumulate" (exercise 1.32) by introducing the notion
;; of a filter on the terms to be combined. That is, combine only those terms derived from values in the
;; range that satisfy a specified condition. The resulting "filtered-accumulate" abstraction takes the
;; same arguments as accumulate, together with an additional predicate of one argument that specifies
;; the filter. Write "filtered-accumulate" as a procedure. Show how to express the following using
;; "filtered-accumulate":
;;
;; (a) the sum of the squares of the prime numbers in the interval a to b.
;;
;; (b) the product of all the positive integers less than n that are relatively prime to n.
;;

;;
;; First, define the "filtered-accumulate" procedure:
;;
(defn filtered-accumulate [combiner null-value term a next-point b predicate]
  (if (> a b)
    null-value
    (combiner (if (predicate a)
                (term a)
                null-value)
              (filtered-accumulate combiner null-value term (next-point a) next-point b predicate))))

;;
;; Define the prime predicate (take it from Exercise 1.28, which is the fastest prime testing procedure we developed):
;;
(defn square [n] (* n n))

(defn random [n]
  (Math/floor (rand n)))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
        :else (rem (* base (expmod base (- exp 1) m)) m)))

(defn prime? [n]
  (defn get-random-a []
    (+ 2 (random (- n 4))))
  (defn test-value [a]
    (= (expmod a (- n 1) n) 1))
  (cond (= n 1) false
        (= n 2) true
        (= n 3) true
        (= n 4) false
        (= n 5) true
        :else
        (and (test-value (- n 1))
             (test-value (- n 2))
             (test-value (get-random-a))
             (test-value (get-random-a))
             (test-value (get-random-a)))))

;; do the "prime" test
(defn sum-of-prime-squares [a b]
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-prime-squares 1 1)
;; --> 0

(sum-of-prime-squares 1 2)
;; --> 4

(sum-of-prime-squares 1 3)
;; --> 13

(sum-of-prime-squares 1 4)
;; --> 13

(sum-of-prime-squares 1 5)
;; --> 38

;;
;; We know that 1000999 is prime..
;;
(def test-value 1000999)
(= (square test-value) (sum-of-prime-squares test-value (+ test-value 1)))

;; define the "gcd" test
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn relatively-prime? [a b]
  (= (gcd a b) 1))

(defn prime-product [n]
  (defn relatively-prime-to-n? [a]
    (relatively-prime? a n))
  (filtered-accumulate * 1 identity 1 inc n relatively-prime-to-n?))

;;
;; Let's run through some unit tests:
;;
(prime-product 3)
;; ==> 2

;;
;; All integers greater than 1 are relatively prime to 1.
;; 3 is r.p. to 2 as well.
;;
;; Hence, (= (prime-product 3) 2) as expected.
;;

(prime-product 4)
;; ==> 3

;;
;; 4 is r.p. to 1 and 3.
;; (= (prime-product 4) (* 1 3))
;;

(prime-product 5)
;; ==> 24

;;
;; 5 is r.p. to 1, 2, 3 and 4.
;; (= (prime-product 5) (* 1 2 3 4))
;;

(prime-product 6)
;; ==> 5

;;
;; 6 is r.p. is 1 and 5.
;; (= (prime-product 6) (* 1 5))
;;

(prime-product 7)
;; ==> 720

;;
;; 7 is r.p. to 1, 2, 3, 4, 5, 6
;; (= (prime-product 7) (* 1 2 3 4 5 6))
;;

(prime-product 8)
;; ==> 105

;;
;; 8 is r.p. to 1, 3, 5, 7
;; (= (prime-product 8) (* 1 3 5 7))
;;

(prime-product 9)
;; ==> 2240

;;
;; 9 is r.p. is 2, 4, 5, 7, 8
;; (= (prime-product 9) (* 1 2 4 5 7 8))
;;

(prime-product 10)
;; ==> 189

;;
;; 10 is r.p. to 3, 7, 9
;; (= (prime-product 10) (* 1 3 7 9)
;;

;;
;; It's interesting to note that if n is prime, our prime product routine returns (n-1)!
;;
;; Vide 3, 5 and 7 as examples.
;;

;;
;; One thing we would expect is for (prime-product p) to be equal to (factorial (- p 1)),
;; since p will be relatively prime to all positive integers less than p.
;;
;; Let's define a factorial procedure to test this for some primes.
;;
(defn factorial [n]
  (cond (= n 0) 1
        (= n 1) 1
        :else (* n (factorial (- n 1)))))

(= 1 (prime-product 2))
;; --> true

(= 2 (prime-product 3))
;; --> true

(= (factorial 4) (prime-product 5))
;; --> true

(= (factorial 6) (prime-product 7))
;; --> true

(= (factorial 10) (prime-product 11))
;; --> true

(= (factorial 12) (prime-product 13))
;; --> true

(= (factorial 1008) (prime-product 1009))
;; --> true

;;
;; Let's test some smaller composite numbers.
;;
;; 4 is relatively prime to 3
(= 3 (prime-product 4))
;; --> true

;; 6 is relatively prime to 5
(= 5 (prime-product 6))
;; --> true

;; 8 is relatively prime to 3, 5 and 7
(= (* 3 5 7) (prime-product 8))
;; --> true

;; 9 is relatively prime to 2, 4, 5, 7 and 8
(= (* 2 4 5 7 8) (prime-product 9))
;; --> true

;; 10 is relatively prime to 3, 7 and 9
(= (* 3 7 9) (prime-product 10))
;; --> true