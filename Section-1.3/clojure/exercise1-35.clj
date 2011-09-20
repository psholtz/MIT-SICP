;;
;; Exercise 1.35
;;
;; Show that the golden ratio phi (section 1.2.2) is a fixed point of the transformation x -> 1 + 1/x,
;; and use this fact to compute phi by means of the fixed-point procedure.
;;

;;
;; Define the "fixed-point" procedure.
;;
;; The text defines "fixed-point" in terms of a sub-procedure named "try".
;;
;; We cannot use this name in Clojure, owing to the fact that "try" is a
;; reserved word in Clojure, stemming from the try/catch blocks of Java.
;;
;; Instead, we name the inner sub-procedure "try-guess":
;;
(defn fixed-point [f first-guess]
  (def tolerance 0.00001)
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))

;;
;; Define "phi" as a fixed point:
;;
(def phi (fixed-point (fn [y] (+ 1.0 (/ 1.0 y))) 1.0))

phi
;; ==> 1.6180327868852458


