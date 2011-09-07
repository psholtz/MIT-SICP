;; ++++++++++++++++++++++++ 
;; PROJECT 1
;;
;; Clojure implementation.
;; ++++++++++++++++++++++++
(defn square [x] (* x x))

(def gravity 9.8)  ;; in m/s
(def pi (Math/PI))

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

;;
;; Try (root1 5 3 6):
;;
(root1 5 3 6)
;; ==> ()

;;
;; Returns nil, since the roots to this polynomial are complex.
;;

;; +++++++++++++++++++++++
;; PROBLEM 3
;;
;; Calculate flight times.
;; +++++++++++++++++++++++
;;
;; The polynomial whose roots we are attempting to calculate is:
;;
;; (0.5)*a*t^2 + v*t + x0 = 0
;;
;; which is the equation of motion for a particle moving under constant acceleration a,
;; constant velocity v and initial position x0.
;;
;; In our case, the acceleration is due to gravity (9.8) and is directed downwards
;; (i.e., negative). The initial velocity and position (i.e., elevation) are passed
;; in as parameters to the procedure.
;;
;; If we plot the elevation of the ball versus time, where the time coordinate is
;; used as the abscissa, and the elevation of the ball is used as the ordinate, the
;; "root1" procedure we defined above will give the "left-most" point at which the
;; graph intersects the abscissa (i.e., t-axis), while the "root2" procedure will
;; give the "right-most" point.
;;
;; We are seeking the "right-most" root of this polynomial, since we are seeking the
;; point "forward" in time from where the ball was released, at which the ball strikes
;; the ground (i.e., has elevation 0).
;;
;; For this reason, we will define our procedure using the "root2" procedure.
;;
;; We define the polynomial whose roots we are seeking as the equation of motion above:
;;
(defn time-to-impact [vertical-velocity elevation]
  (root2 (* -0.5 gravity) vertical-velocity elevation))

;;
;; The following unit tests model shooting the ball straight upwards from the ground
;; at various velocities (i.e., elevation = 0 and vertical velocity = 10, 20 and 50 m/s):
;;
(time-to-impact 10 0)
;; ==> 2.041
(time-to-impact 20 0)
;; ==> 4.082
(time-to-impact 50 0)
;; ==> 10.204

;;
;; The harder the ball is shot upwards, the longer it takes to hit the ground.
;;
;; Suppose now we are standing 10m above the ground, and again we shoot the ball straight
;; upwards. How long will it take to hit the ground this time? It will take longer than before,
;; since it must first follow the same arc (i.e., same flight time) as when launched from the
;; ground, but this time it must fall an additional distance before it hits the ground.
;;
;; Let's see if our intuition is born out in the code:
;;
(time-to-impact 10 10)
;; ==> 2.776
(time-to-impact 10 20)
;; ==> 3.284

;;
;; Launching the ball upwards from an initial elevation > 0 causes the overall flight time to
;; lengthen, which is what we expect. Also, the higher our initial elevation, the longer the overall
;; flight time, which is again what we expect.
;;
;; Suppose now that we wish to calculate the flight time, not for when the ball impacts the
;; ground, but rather for when the ball will reach a certain target elevation. Again, we will
;; choose "root2" since we want to know when the ball reaches the target elevation on the "way
;; down" rather than on the "way up".
;;
;; Calculating the time to reach the target elevation is the same as calculating the time to
;; impact, provided that we "start" at an initial elevation which is reduced by an amount equal
;; to the target elevation we are aiming for. We can, therefore, define our procedure in nearly
;; identical terms as before, provided we make this adjustment to the starting elevation from
;; which the ball is launched:
;;
(defn time-to-height [vertical-velocity elevation target-elevation]
  (root2 (* -0.5 gravity) vertical-velocity (- elevation target-elevation)))

;;
;; We expect the "time to impact" and the "time to height 0" procedures to return the same number:
;;
(= (time-to-impact 10 0) (time-to-height 10 0 0))
;; ==> true
(= (time-to-impact 20 0) (time-to-height 20 0 0))
;; ==> true

;;
;; Similarly, when the ball is shot from varying elevations it should still reach the ground at
;; the same time that time that it would reach "height 0":
;;
(= (time-to-impact 10 10) (time-to-height 10 10 0))
;; ==> true
(= (time-to-impact 10 20) (time-to-height 10 20 0))
;; ==> true

;;
;; The higher we aim the ball, the "shorter" the time should be, since the ball does not
;; need to "fall" as far as it would if it were falling all the way to the ground.
;;
(time-to-height 10 20 0)
;; ==> 3.284
(time-to-height 10 20 10)
;; ==> 2.776
(time-to-height 10 20 20)
;; ==> 2.041
(time-to-height 10 20 25)
;; ==> 1.165

;;
;; Is there a height that we can't reach with a given velocity?
;;
;; Yes, there is (which is good):
;;
(time-to-height 10 20 50)
;; ==> ()

;;
;; How about if we increase the velocity? Are we then able to reach the target height?
;;
;; Yes, there is (which is good):
;;
(time-to-height 10 50 50)
;; ==> 2.041

;; +++++++++++++++++
;; PROBLEM 4
;;
;; Flight distance.
;; +++++++++++++++++
;;
;; First define the helper procedures:
;;
(defn degree2radian [deg]
  (/ (* deg pi) 180.0))

(defn meters-to-feet [m]
  (/ (* m 39.6) 12))

(defn feet-to-meters [f]
  (/ (* f 12) 30.6))

(defn hours-to-seconds [h]
  (* h 3600.0))

(defn seconds-to-hours [s]
  (/ s 3600.0))

;;
;; We measure angle in radians, so we have to convert to degrees.
;;
;; We measure evelation and velocity in meters.
;;
;; Then calculate how long the ball will stay in the air, and how far it can
;; travel horizontally during that time, before it hits the ground.
;;
(def travel-distance-simple
  (fn [elevation velocity angle]
    (let [rangle (degree2radian angle)]
      (let [vy (* velocity (Math/sin rangle))
            vx (* velocity (Math/cos rangle))]
        (let [t0 (time-to-impact vy elevation)]
          (let [x0 (position 0 vx 0 t0)]
            x0))))))

;;
;; Let's calculate the distances in meters:
;;
(travel-distance-simple 1 45 0)
;; ==> 20.329
(travel-distance-simple 1 45 45)
;; ==> 207.628
(travel-distance-simple 1 45 90)
;; ==> 2.537e-14

;;
;; This last distance, hit straight up, is essentially zero, which we would expect.
;;

;;
;; Now let's translate these distances into feet:
;;
(meters-to-feet (travel-distance-simple 1 45 0))
;; ==> 67.085
(meters-to-feet (travel-distance-simple 1 45 45))
;; ==> 685.172
(meters-to-feet (travel-distance-simple 1 45 90))
;; ==> 8.371e-14

;; +++++++++++++++++++ 
;; PROBLEM 5
;;
;; Best angle to hit.
;; +++++++++++++++++++
;;
;; We will increment 1 degre at a time:
;;
(def angle-increment 1.0)

;;
;; Define a helper procedure to convert our answers back to angles
;;
(defn radian2angle [rad]
  (/ (* rad 180.0) pi))

;;
;; Define the actual procedure itself:
;;
(defn find-best-angle [velocity elevation]
  (defn find-best-angle-iter [best-distance best-angle test-angle]
    (if (> test-angle 90)
      best-angle
      (let [test-distance (travel-distance-simple elevation velocity test-angle)
            next-angle (+ test-angle angle-increment)]
        (if (> test-distance best-distance)
          (find-best-angle-iter test-distance test-angle next-angle)
          (find-best-angle-iter best-distance best-angle next-angle)))))
  (find-best-angle-iter 0.0 0.0 0.0))

;;
;; Let's step through some sample velocities at the elevation of 1 meter:
;;
(find-best-angle 10 1)
;; ==> 42.0
(find-best-angle 20 1)
;; ==> 44.0
(find-best-angle 30 1)
;; ==> 45.0 
(find-best-angle 40 1)
;; ==> 45.0
(find-best-angle 45 1)
;; ==> 45.0
(find-best-angle 50 1)
;; ==> 45.0

;;
;; Now let's step through these same angles, but at an elevation of 10 meters:
;;
(find-best-angle 10 10)
;; ==> 30.0
(find-best-angle 20 10)
;; ==> 39.0
(find-best-angle 30 10)
;; ==> 42.0
(find-best-angle 40 10)
;; ==> 43.0 
(find-best-angle 45 10)
;; ==> 44.0
(find-best-angle 50 10)
;; ==> 44.0

;;
;; In both cases, the best angle seems to asymptotically approach 45 degrees,
;; although it approaches this limit more slowly when the initial elevation is higher.
;;

;; +++++++++++++++++
;; PROBLEM 6
;;
;; Incorporate drag.
;; +++++++++++++++++
;;
;; Define some constants
;;
(def drag-coeff 0.5)
(def density 1.25)
(def mass 0.145)
(def diameter 0.074)
(def beta (* 0.5 drag-coeff density (* pi 0.25 (square diameter))))

;;
;; Define the integrate procedure:
;;
(defn integrate [x0 y0 u0 v0 g m beta]
  '())