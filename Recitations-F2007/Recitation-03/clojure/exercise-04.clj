;;
;; Exericse 4
;;
;; Write an iterative procedure that computes e.
;;

;;
;; Define the "factorial" procedure:
;;
(defn fact [n]
  (cond (= n 0) 1
        :else
        (* n (fact (- n 1)))))

;;
;; Define an iterative version of the "e" procedure:
;;
(defn e []
  (def tolerance 0.00001)
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn term [n]
    (/ 1.0 (fact n)))
  (defn e-iter [c t]
    (let [next (+ t (term c))]
      (if (close-enough? t next)
        next
        (e-iter (+ c 1) next))))
  (e-iter 0 0))

;;
;; Unit test:
;;
(e)
;; ==> 2.7182815255731922