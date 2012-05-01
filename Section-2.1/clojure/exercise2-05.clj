;;
;; Exercise 2.5
;;
;; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations
;; if we represent the pair a and b as the integer that is the product 2^a*3^b. Give the corresponding
;; definitions of the procedures cons, car and cdr.
;;

;;
;; Definition for the cons procedure:
;;
(defn cons [a b] (* (Math/pow 2 a) (Math/pow 3 b)))

;;
;; The definitions for car and cdr can
;; be given either as recursive or
;; iterative processes.
;;
;; The following procedures generate a recursive process:
;;
(defn car [n]
  (if (= (rem n 2) 0)
    (+ 1 (car (/ n 2)))
    0))

(defn cdr [n]
  (if (= (rem n 3) 0)
    (+ 1 (cdr (/ n 3)))
    0))

;;
;; The following procedures generate an iterative process:
;;
(defn car [n]
  (defn car-iter [n c]
    (if (= (rem n 2) 0)
      (car-iter (/ n 2) (+ 1 c))
      c))
  (car-iter n 0))

(defn cdr [n]
  (defn cdr-iter [n c]
    (if (= (rem n 3) 0)
      (cdr-iter (/ n 3) (+ 1 c))
      c))
  (cdr-iter n 0))