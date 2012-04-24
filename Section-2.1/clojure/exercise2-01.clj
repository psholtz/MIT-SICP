
;;
;; Modified version of the "make-rat" constructor, incorporating the "gcd" test as in text:
;;
(defn make-rat
  {:doc "Construct a rational number where n is the numerator, and d is the denominator."}
  [n d]
  
  (defn gcd
    {:doc "Return the greatest common divisor of a and b."}
    [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))))
  
  (defn negative?
    {:doc "Return true if the number is less than zero, otherwise false."}
    [x]
    (< x 0))
  
  (let [g (gcd (Math/abs n) (Math/abs d))]
    (if (negative? d)
      ;;
      ;; Clojure has a different notion of "cons" than traditional Lisps do,
      ;; hence we "close" the cons with a "list".. 
      ;;
      (cons (/ (* -1 n) g) (list (/ (* -1 d) g)))
      (cons (/ n g) (list (/ d g))))))
 
;;
;; Let's define the "print-rat" procedure so we can run some unit tests.
;;
(defn print-rat
  {:doc "Prints out a representation of the rational number."}
  [x]
  (print (numer x))
  (print "/")
  (print (denom x))
  (println ""))

;;
;; Need selectors for numerator and denominator as well:
;;
(defn numer {:doc "Extract numerator of rational number,"} [x] (first x))
(defn denom {:doc "Extract denominator of rational number."} [x] (second x))