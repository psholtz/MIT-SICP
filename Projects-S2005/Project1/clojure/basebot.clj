;; ++++++++++++++++++++++++ 
;; PROJECT 1
;;
;; Clojure implementation.
;; ++++++++++++++++++++++++
(defn square [x] (* x x))

(def gravity 9.8)  ;; in m/s
(def pi 3.14159)

;; ++++++++++++++++++++++++++++ 
;; PROBLEM 1
;;
;; Simple kinematics equation.
;; ++++++++++++++++++++++++++++
(def position
  (fn [a v u t]
    (+
     (* 0.5 a (square t))
     (* v t)
     u)))

;;
;; Run some unit tests
;;
(= (position 0 0 0 0) 0)
(= (position 0 0 20 0) 20)
(= (position 0 5 10 10) 60)
(= (position 2 2 2 2) 10)
(= (position 5 5 5 5) 92.5)
(= (position 0 -10 0 1) -10)

;; +++++++++++++++++++++++++++++ 
;; PROBLEM 2
;;
;; Implement quadratic formula.
;; +++++++++++++++++++++++++++++
(defn discriminant [a b c]
  (- (square b) (* 4 a c)))

(defn root1 [a b c]
  (let [d (discriminant a b c)]
    (if (< d 0)
      '()
      (/ (+ (* -1 b) (Math/sqrt d)) (* 2 a)))))

(defn root2 [a b c]
  (let [d (discriminant a b c)]
    (if (< d 0)
      '()
      (/ (- (* -1 b) (Math/sqrt d)) (* 2 a)))))

;;
;; Run some unit tests
;;
(= (discriminant 0 5 0) 25)
