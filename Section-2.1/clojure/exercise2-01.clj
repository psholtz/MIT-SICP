
;;
;; Modified version of the "make-rat" constructor, incorporating the "gcd" test as in text:
;;
(defn make-rat [n d]
  (defn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))))
  (defn negative? [x]
    (< x 0))
  (let [g (gcd (Math/abs n) (Math/abs d))]
    (if (negative? d)
      (cons (/ (* -1 n) g) (list (/ (* -1 d) g)))
      (cons (/ n g) (list (/ d g))))))
 
;;
;; Let's define the "print-rat" procedure so we can run some unit tests.
;;
(defn print-rat [x]
  (print (numer x))
  (print "/")
  (print (denom x))
  (println ""))

;;
;; Need selectors for numerator and denominator as well:
;;
(defn numer [x] (first x))
(defn denom [x] (second x))