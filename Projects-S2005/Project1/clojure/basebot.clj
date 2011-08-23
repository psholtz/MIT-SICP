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
(= (discriminant 5 0 0) 0)
(= (discriminant 0 0 5) 0)
(= (discriminant 3 6 3) 0)

(= (root1 1 2 1) -1)
(= (root2 1 2 1) -1)
(= (root1 1 -2 1) 1)
(= (root2 1 -2 1) 1)

;; +++++++++++++++++++++ 
;; PROBLEM 3
;;
;; Calculate distances.
;; +++++++++++++++++++++
(defn time-to-impact [vertical-velocity elevation]
  (root2 (* -0.5 gravity) vertical-velocity elevation))

(defn time-to-height [vertical-velocity elevation target-elevation]
  (root2 (* -0.5 gravity) vertical-velocity (- elevation target-elevation)))

;; +++++++++++++++++
;; PROBLEM 4
;;
;; Flight distance.
;; +++++++++++++++++
(defn degree2radian [deg]
  (/ (* deg pi) 180.0))

(defn travel-distance-simple [elevation velocity angle]
  (let [rangle (degree2radian angle)]
    (let [vy (* velocity (Math/sin rangle))
          vx (* velocity (Math/cos rangle))]
      (let [t0 (time-to-impact vy elevation)]
        (let [x0 (position 0 vx 0 t0)]
          x0)))))

(defn meters-to-feet [m]
  (/ (* m 39.6) 12.0))

(defn feet-to-meters [f]
  (/ (* f 12.0) 39.6))

(defn hours-to-seconds [h]
  (* h 3600.0))

(defn seconds-to-hours [s]
  (/ s 3600.0))

;; +++++++++++++++++++ 
;; PROBLEM 5
;;
;; Best angle to hit.
;; +++++++++++++++++++
(def angle-increment 1.0) ;; use degrees

(defn radian2degree [rad] (/ (* rad 180.0) pi))

(defn find-best-angle-iter [velocity elevation best-distance best-angle test-angle]
  (if (> test-angle 90)
    best-angle
    (let [test-distance (travel-distance-simple elevation velocity test-angle)
          next-angle (+ test-angle angle-increment)]
      (if (> test-distance best-distance)
        (find-best-angle-iter velocity elevation test-distance test-angle next-angle)
        (find-best-angle-iter velocity elevation best-distance best-angle next-angle)))))

;; +++++++++++++++++ 
;; PROBLEM 6
;;
;; Incorporate drag
;; +++++++++++++++++
(def drag-coeff 0.5)
(def density 1.25)
(def mass 0.145)
(def diameter 0.074)
(def beta (* 0.5 drag-coeff density (* pi 0.25 (square diameter))))

(defn integrate (x0 y0 u0 v0 g m beta)
  (if (< y0 0)
    x0
    (let [dt 0.1
          speed (Math/sqrt (+ (square u0) (square v0)))]
      (let [drag (* beta (square speed))]
        (let [dx (* u0 dt)
              dy (* v0 dt)
              du (* (/ -1.0 m) speed beta u0 dt)
              dv (* -1.0 (+ (* (/ 1.0 m) speed beta v0) g) dt)]
          (integrate (+ x0 dx)
                     (+ y0 dy)
                     (+ u0 du)
                     (+ v0 dv)
                     g m beta))))))

(defn travel-distance [elevation velocity angle]
  (let [rangle (degree2radian angle)]
    (let [vy (* velocity (Math/sin rangle))
          vx (* velocity (Math/cos rangle))]
      (integrate 0 elevation vx vy gravity mass beta))))

