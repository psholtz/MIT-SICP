;; ++++++++++++++++++++++++ 
;; PROJECT 1
;;
;; Clojure implementation.
;; ++++++++++++++++++++++++
(defn
  ^{:doc "Return the square of the argument."
    :test (do
            (assert (= 1 (square 1)))
            (assert (= 4 (square 2)))
            (assert (= 9 (square 3))))}
  square [x] (* x x))

(def gravity 9.8)  ;; in m/s
(def pi (Math/PI))

;; ++++++++++++++++++++++++++++ 
;; PROBLEM 1
;;
;; Simple kinematics equation.
;; ++++++++++++++++++++++++++++
(def
  ^{:doc "Return position of particle subject to acceleration a, velocity v, position u at time t."
    :test (do
            (assert (= (position 0 0 0 0) 0))
            (assert (= (position 0 0 20 0) 20))
            (assert (= (position 0 5 10 10) 60))
            (assert (= (position 2 2 2 2) 10))
            (assert (= (position 5 5 5 5) 92.5))
            (assert (= (position 0 -10 0 1) -10)))}          
  position
  (fn [a v u t]
    (+
     (* 0.5 a (square t))
     (* v t)
     u)))

;; +++++++++++++++++++++++++++++ 
;; PROBLEM 2
;;
;; Implement quadratic formula.
;; +++++++++++++++++++++++++++++
(defn
  ^{:doc "Quadratic discriminant for constants a, b, c."
    :test (do
            (assert (= 25 (discriminant 0 5 0)))
            (assert (= 0 (discriminant 0 0 0)))
            (assert (= 0 (discriminant 0 0 5)))
            (assert (= 0 (discriminant 3 6 3))))}
  discriminant [a b c]
  (- (square b) (* 4 a c)))

(defn
  ^{:doc "First quadratic root for constants a, b, c"
    :test (do
            ;; basic tests
            (assert (= -1 (root1 1 2 1)))
            (assert (= +1 (root1 1 -2 1)))

            ;; return nil, since roots are complex
            (assert (= '() (root1 5 3 6))))}
  root1 [a b c]
  (let [d (discriminant a b c)]
    (if (< d 0)
      '()
      (/ (+ (* -1 b) (Math/sqrt d)) (* 2 a)))))

(defn
  ^{:doc "Second quadratic root for constants a, b, c"
    :test (do
            ;; basic tests
            (assert (= -1 (root2 1 2 1)))
            (assert (= +1 (root2 1 -2 1))))}
  root2 [a b c]
  (let [d (discriminant a b c)]
    (if (< d 0)
      '()
      (/ (- (* -1 b) (Math/sqrt d)) (* 2 a)))))

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
(defn
  ^{:doc "Determine time it takes to strike the ground, starting at elevation, launched with vertical-velocity upwards."
    :test (do
            ;;
            ;; The following unit tests model shooting the ball straight upwards from the ground
            ;; at various velocities (i.e., elevation = 0 and vertical velocity = 10, 20 and 50 m/s):
            ;;
            (def tolerance 0.001)
            (assert (< (Math/abs (- 2.041 (time-to-impact 10 0))) tolerance))
            (assert (< (Math/abs (- 4.082 (time-to-impact 20 0))) tolerance))
            (assert (< (Math/abs (- 10.204 (time-to-impact 50 0))) tolerance))

            ;;
            ;; The harder the ball is shot upwards, the longer it takes to hit the ground.
            ;;
            ;; Suppose now we are standing 10m above the ground, and again we shoot the ball straight
            ;; upwards. How long will it take to hit the ground this time? It will take longer than
            ;; before, since it must first follow the same arc (i.e., the same flight time) as when
            ;; launched from the ground, but this time it must fall an additional distance before
            ;; it hits the ground.
            ;;
            (assert (< (Math/abs (- 2.776 (time-to-impact 10 10))) tolerance))
            (assert (< (Math/abs (- 3.284 (time-to-impact 10 20))) tolerance))) }
  
  time-to-impact [vertical-velocity elevation]
  (root2 (* -0.5 gravity) vertical-velocity elevation))

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
(defn
  ^{:doc "Determine time it takes to reach target-elevation, starting at elevation, launched upwards with vertical-velocity."
    :test (do
            ;;
            ;; We expect the "time to impact" and the "time to height 0" procedures to return the same number:
            ;;
            (assert (= (time-to-impact 10 0) (time-to-height 10 0 0)))
            (assert (= (time-to-impact 20 0) (time-to-height 20 0 0)))

            ;;
            ;; Similarly, when the ball is shot from varying elevations it should still reach the ground
            ;; at the same time that it would reach "height 0":
            ;;
            (assert (= (time-to-impact 10 10) (time-to-height 10 10 0)))
            (assert (= (time-to-impact 10 20) (time-to-height 10 20 0)))

            ;;
            ;; The higher we aim the ball, the "shorter" the time should be, since the ball does not
            ;; need to "fall" as far as it would if it were falling all the way to the ground.
            ;;
            (assert (< (Math/abs (- 3.284 (time-to-height 10 20 0)))))
            (assert (< (Math/abs (- 2.776 (time-to-height 10 20 10)))))
            (assert (< (Math/abs (- 2.041 (time-to-height 10 20 20)))))
            (assert (< (Math/abs (- 1.165 (time-to-height 10 20 25)))))

            ;;
            ;; Is there a height that we can't reach with a given velocity?
            ;;
            (assert (= '() (time-to-height 10 20 50)))

            ;;
            ;; How about if we increase the velocity? Are we then able to reach the target height?
            ;;
            (assert (< (Math/abs (- 2.041 (time-to-height 10 50 50)))))) }
  
  time-to-height [vertical-velocity elevation target-elevation]
  (root2 (* -0.5 gravity) vertical-velocity (- elevation target-elevation)))

;; +++++++++++++++++
;; PROBLEM 4
;;
;; Flight distance.
;; +++++++++++++++++
;;
;; First define the helper procedures:
;;
(defn
  ^{:doc "Calculate degrees for argument radians."
    :test (do
            (def tolerance 0.001)
            (assert (= 0 (degree2radian 0)))
            (assert (< (Math/abs (- (/ pi 4) (degree2radian 45))) tolerance))
            (assert (< (Math/abs (- (/ pi 2) (degree2radian 90))) tolerance))
            (assert (< (Math/abs (- (* (/ pi 4) 3) (degree2radian 135))) tolerance))
            (assert (< (Math/abs (- pi (degree2radian 180))) tolerance))
            (assert (< (Math/abs (- (* (/ pi 4) 5) (degree2radian 225))) tolerance))
            (assert (< (Math/abs (- (* (/ pi 2) 3) (degree2radian 270))) tolerance))
            (assert (< (Math/abs (- (* (/ pi 4) 7) (degree2radian 315))) tolerance))) }
  
  degree2radian [deg]
  (/ (* deg pi) 180.0))

(defn
  ^{:doc "Calculate feet for argument meters."
    :test (do
            (assert (= 1 1))) }
  meters-to-feet [m]
  (/ (* m 39.6) 12))

(defn
  ^{:doc "Calculate meters for argument feet."
    :test (do
            (assert (= 1 1))) }
  feet-to-meters [f]
  (/ (* f 12.0) 39.6))

(defn
  ^{:doc "Calculate seconds for argument hours."
    :test (do
            (assert (= 1 1))) }
  hours-to-seconds [h]
  (* h 3600.0))

(defn
  ^{:doc "Calculate hours for argument seconds."
    :test (do
            (assert (= 1 1))) }
  seconds-to-hours [s]
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

;;
;; Use the "integreate" procedure to calculate the travel-distance, incorporating drag:
;;
(defn travel-distance [elevation velocity angle]
  (let [rangle (degree2radian angle)]
    (let [vy (* velocity (Math/sin rangle))
          vx (* velocity (Math/cos rangle))]
      (integrate 0 elevation vx vy gravity mass beta))))

;;
;; Run the proposed unit tests, answers are given in meters:
;;
(travel-distance 1 45 45)
;; ==> 92.508
(travel-distance 1 45 40)
;; ==> 95.337
(travel-distance 1 45 35)
;; ==> 94.206

;;
;; Let's build a procedure to answer the question, "for what range of angles will
;; the ball land over the fence, if the fence is 300 feet from home plate and the
;; ball is hit at 45 m/s?". First we build a procedure to find the "extremum" in a
;; list. To find "maximum" we invoke the procedure with the comparator ">", and to
;; find the "minimum" we invoke the procedure with the comparator "<":
;;
(defn extremum-in-list [list-a comparator limit]
  (defn extremum-in-list-iter [list-b extreme-element]
    (if (= (count list-b) 0)
      extreme-element
      (let [test (first list-b)]
        (if (comparator test extreme-element)
          (extremum-in-list-iter (rest list-b) test)
          (extremum-in-list-iter (rest list-b) extreme-element)))))
  (extremum-in-list-iter list-a limit))

;;
;; Run some unit tests to see how well the procedure works.
;;
;; Find the max element:
;;
(extremum-in-list (list 1 2 4 50 2 5) > -1)
;; ==> 50

;;
;; Find the min element:
;;
(extremum-in-list (list 1 2 4 50 2 5) < 51)
;; ==> 1

;;
;; Find the range of angles for which a ball launched at a given elevation
;; and a given velocity will travel to or beyond the target distance.
;;
(defn range-of-angles [elevation velocity target-distance]

  ;;
  ;; This procedure will produce a list of *all* angles which
  ;; will travel to or beyond the target distance.
  ;;
  (defn range-of-angles-iter [angles angle]
    (if (> angle 90)
      angles
      (let [next-angle (+ angle angle-increment)]
        (if (> (travel-distance elevation velocity angle) target-distance)
          (range-of-angles-iter (cons angle angles) next-angle)
          (range-of-angles-iter angles next-angle)))))
  
  ;;
  ;; Take the list of *all* angles, and return an ordered pair of the
  ;; max and min elements from that list. We will return this pair, which
  ;; will represent the min and max angles at which the ball can be hit
  ;; at the given elevation at the given velocity, and travel to or beyond
  ;; the target distance:
  ;;
  (let [range (range-of-angles-iter () 0)]
    (list
     (extremum-in-list range < 91)
     (extremum-in-list range > -1))))

;;
;; Now answer the question:
;;
;; Suppose the outfield fence is 300 ft from home plate, and the ball is hit at 45 m/s.
;; For what range of angles will the ball land over the fense?
;;
(range-of-angles 1 45 (feet-to-meters 300))
;; ==> (28.0 48.0)

;;
;; Hence, the answer is, for angles between 28 degrees and 48 degrees, the ball
;; will land over the fence. We can verify:
;;
(meters-to-feet (travel-distance 1 45 27))
;; ==> 297.520
(meters-to-feet (travel-distance 1 45 28))
;; ==> 300.596
(meters-to-feet (travel-distance 1 45 48))
;; ==> 301.417
(meters-to-feet (travel-distance 1 45 49))
;; ==> 296.486

;;
;; Let's run through these same examples, as though in Denver, rather than in Boston.
;;
;; We adjust the density of air down from 1.25 to 1.06:
;;
(def density 1.06)
(def beta (* 0.5 drag-coeff density (* pi 0.25 (square diameter))))

(meters-to-feet (travel-distance 1 45 45))
;; ==> 332.960
(meters-to-feet (travel-distance 1 45 40))
;; ==> 339.029
(meters-to-feet (travel-distance 1 45 35))
;; ==> 334.671

(range-of-angles 1 45 (feet-to-meters 300))
;; ==> (24.0 55.0)

;;
;; So this time, the range of angles for which the ball can travel 300 ft is substantially
;; larger than in Boston. This makes sense, it's what we would expect.
;;


;;
;; Let's check the boundaries:
;;
(meters-to-feet (travel-distance 1 45 23))
;; ==> 297.602
(meters-to-feet (travel-distance 1 45 24))
;; ==> 308.402
(meters-to-feet (travel-distance 1 45 55))
;; ==> 300.430
(meters-to-feet (travel-distance 1 45 56))
;; ==> 296.813

;;
;; Let's go "back to Boston" for the rest of this exercise:
;;
(def density 1.25)
(def beta (* 0.5 drag-coeff density (* pi 0.25 (square diameter))))

;; +++++++++++++++++++++++++
;; PROBLEM 7
;;
;; Aim and throw the ball.
;; +++++++++++++++++++++++++
;;
;; Define the integration procedure iteratively.
;;
;; Procedure will return the time needed to throw the ball the given distance, at
;; the given velocity, from the given elevation.
;;
(defn throw-ball [elevation velocity distance]
  (defn throw-ball-iter [shortest-time test-angle]
    (if (> test-angle 90)
      shortest-time
      (let [sample-distance (travel-distance elevation velocity test-angle)
            radian-angle (degree2radian test-angle)
            next-angle (+ test-angle angle-increment)
            tolerance 5]
        (if (< (Math/abs (- sample-distance distance)) tolerance)
          (let [vy (* velocity (Math/sin radian-angle))]
            (let [t0 (time-to-impact vy elevation)]
              (if (or (= shortest-time 0) (< t0 shortest-time))
                (throw-ball-iter t0 next-angle)
                (throw-ball-iter shortest-time next-angle))))
          (throw-ball-iter shortest-time next-angle)))))
  (throw-ball-iter 0 -90))

;;
;; Convert from miles-per-hour to meters-per-second:
;;
(defn mph-to-ms [mph]
  (feet-to-meters (* (seconds-to-hours mph) 5280)))

;;
;; Answer the questions:
;;
;; How long does it take for a ball thrown from home plate at 45 m/s to reach second base?
;;
(throw-ball 1 45 36)
;; ==> 0.640

;;
;; How long does it take for a ball thrown from home plate at 35 m/s to reach second base?
;;
(throw-ball 1 35 36)
;; ==> 0.959

;;
;; How long does it take for a ball thrown from home plate at 55 m/s to reach second base?
;;
(throw-ball 1 55 36)
;; ==> 0.560

;;
;; These numbers make sense: the slower the ball is thrown the longer it takes to get to second base.
;;

;;
;; If the pitcher throws at 90 mph, how long does it take to reach home?
;;
;; If the distance between home plate and second base is 36m, we will take the
;; distance between home and the pitcher's mound to be half that, or 18m.
;;
(throw-ball 1 (mph-to-ms 90) 18)
;; ==> 0.286

;;
;; How long does it take a ball thrown by the catcher at 90 mph to reach second base?
;;
(throw-ball 1 (mph-to-ms 90) 36)
;; ==> 0.819

;;
;; Note that a really good base runner should be able to get from first to second base in roughly 3 seconds.
;; If the pitcher is throwing at 90 mph how long does it take to each home? If the catcher throws at 90 mph,
;; how much time does he have to catch and release the ball if he is going to put out a runner trying to steal
;; second?
;;

;;
;; Given the parameters of the problem, the total time the catcher has is given by the following expression:
;;
(- 3
   (+ (throw-ball 1 (mph-to-ms 90) 18)
      (throw-ball 1 (mph-to-ms 90) 36)))

;; ==> 1.895

;;
;; Now use your procedures to get some data on outfielders. Suppose an outfielder has a strong arm and can
;; throw at 45 m/s. How quickly can he throw the ball to a target at a distance of 30m? 60m? 90m? What if
;; he can thow at 55 m/s?
;;

;;
;; Throws at 45 m/s:
;;
(throw-ball 1 45 30)
;; ==> 0.539
(throw-ball 1 45 60)
;; ==> 1.567
(throw-ball 1 45 90)
;; ==> 3.644

;;
;; Throws at 55 m/s:
;;
(throw-ball 1 55 30)
;; ==> 0.452
(throw-ball 1 55 60)
;; ==> 1.327
(throw-ball 1 55 90)
;; ==> 2.789

;;
;; Throws at 35 m/s:
;;
(throw-ball 1 35 30)
;; ==> 0.676
(throw-ball 1 35 60)
;; ==> 2.296
(throw-ball 1 35 90)
;;==> 0

;;
;; Note that the weaker outfielder is not able to throw the ball 90 m in the air.
;;

;; ++++++++++++++++++ 
;; PROBLEM 8
;;
;; Add some bounces.
;; ++++++++++++++++++
(defn travel-distance-with-bounce [elevation velocity angle bounces]
  (defn travel-distance-with-bounce-iter [elevation velocity angle maximum count total-distance]
    (let [bounce-distance (travel-distance elevation velocity angle)]
      (let [new-total-distance (+ total-distance bounce-distance)]
        (if (= count maximum)
          new-total-distance
          (travel-distance-with-bounce-iter
            0
            (/ velocity 2.0)
            angle
            maximum
            (+ count 1)
            new-total-distance)))))
  (travel-distance-with-bounce-iter elevation velocity angle bounces 0 0))

;;
;; In the first place, we would expect the travel distance with 0 bounces
;; to be the same as the distance given by the "travel-distance" procedure
;; derived above.
;;
;; Let's test this for a range of angles and velocities:
;;
(= (travel-distance 1 45 45) (travel-distance-with-bounce 1 45 45 0))
;; ==> true
(= (travel-distance 1 45 30) (travel-distance-with-bounce 1 45 30 0))
;; ==> true
(= (travel-distance 1 45 60) (travel-distance-with-bounce 1 45 60 0))
;; ==> true

(= (travel-distance 1 35 45) (travel-distance-with-bounce 1 35 45 0))
;; ==> true
(= (travel-distance 1 35 30) (travel-distance-with-bounce 1 35 30 0))
;; ==> true
(= (travel-distance 1 35 60) (travel-distance-with-bounce 1 35 60 0))
;; ==> true


(= (travel-distance 1 55 45) (travel-distance-with-bounce 1 55 45 0))
;; ==> true
(= (travel-distance 1 55 30) (travel-distance-with-bounce 1 55 30 0))
;; ==> true
(= (travel-distance 1 55 60) (travel-distance-with-bounce 1 55 60 0))
;; ==> true

;;
;; This is good, it's what we expect.
;;

;;
;; The question also asks us to model how far the ball gets on an arbitrary
;; number of bounces, until it stops moving. In order to do this, we will
;; compare the distance traveled on two successive bounces. If the resulting
;; change in distance is less than some pre-defined tolerance, we suppose
;; that we have found the distance traveled on an "arbitrary" number of bounces:
;;
(defn travel-distance-with-infinite-bounces [elevation velocity angle]
  (def tolerance 0.001)
  (defn travel-distance-with-infinite-bounces-iter [number-of-bounces]
    (let [distance1 (travel-distance-with-bounce elevation velocity angle number-of-bounces)
          distance2 (travel-distance-with-bounce elevation velocity angle (+ number-of-bounces 1))]
      (if (< (Math/abs (- distance1 distance2)) tolerance)
        distance2
        (travel-distance-with-infinite-bounces-iter (+ number-of-bounces 1)))))
  (travel-distance-with-infinite-bounces-iter 0))

;;
;; Now let's run through the same set of velocities and angles, and see
;; how far the ball gets on 0, 1, 2 and "infinite" bounces.
;;
;; Velocity 45 m/s, Angle 45 degrees:
;;
(travel-distance-with-bounce 1 45 45 0)
;; ==> 92.508
(travel-distance-with-bounce 1 45 45 1)
;; ==> 132.129
(travel-distance-with-bounce 1 45 45 2)
;; ==> 144.792
(travel-distance-with-infinite-bounces 1 45 45)
;; ==> 150.524

;;
;; Velocity 45 m/s, Angle 30 degrees:
;;
(travel-distance-with-bounce 1 45 30 0)
;; ==> 92.732
(travel-distance-with-bounce 1 45 30 1)
;; ==> 130.039
(travel-distance-with-bounce 1 45 30 2)
;; ==> 142.022
(travel-distance-with-infinite-bounces 1 45 30)
;; ==> 146.961

;;
;; Velocity 45 m/s, Angle 60 degrees:
;;
(travel-distance-with-bounce 1 45 60 0)
;; ==> 76.314
(travel-distance-with-bounce 1 45 60 1)
;; ==> 109.497
(travel-distance-with-bounce 1 45 60 2)
;; ==> 120.506
(travel-distance-with-infinite-bounces 1 45 60)
;; ==> 124.838

;;
;; Velocity 55 m/s, Angle 45 degrees:
;;
(travel-distance-with-bounce 1 55 45 0)
;; ==> 111.707
(travel-distance-with-bounce 1 55 45 1)
;; ==> 163.838
(travel-distance-with-bounce 1 55 45 2)
;; ==> 182.351
(travel-distance-with-infinite-bounces 1 55 45)
;; ==> 189.797

;;
;; Velocity 55 m/s, Angle 30 degrees:
;;
(travel-distance-with-bounce 1 55 30 0)
;; ==> 112.975
(travel-distance-with-bounce 1 55 30 1)
;; ==> 163.001
(travel-distance-with-bounce 1 55 30 2)
;; ==> 179.530
(travel-distance-with-infinite-bounces 1 55 30)
;; ==> 186.419

;;
;; Velocity 55 m/s, Angle 60 degrees:
;;
(travel-distance-with-bounce 1 55 60 0)
;; ==> 91.192
(travel-distance-with-bounce 1 55 60 1)
;; ==> 135.481
(travel-distance-with-bounce 1 55 60 2)
;; ==> 151.017
(travel-distance-with-infinite-bounces 1 55 60)
;; ==> 157.373

;;
;; Velocity 35 m/s, Angle 45 degrees:
;;
(travel-distance-with-bounce 1 35 45 0)
;; ==> 71.585
(travel-distance-with-bounce 1 35 45 1)
;; ==> 98.366
(travel-distance-with-bounce 1 35 45 2)
;; ==> 106.668
(travel-distance-with-infinite-bounces 1 35 45)
;; ==> 110.271

;;
;; Velocity 35 m/s, Angle 30 degrees:
;;
(travel-distance-with-bounce 1 35 30 0)
;; ==> 69.756
(travel-distance-with-bounce 1 35 30 1)
;; ==> 95.232
(travel-distance-with-bounce 1 35 30 2)
;; ==> 102.564
(travel-distance-with-infinite-bounces 1 35 30)
;; ==> 106.044

;;
;; Velocity 35 m/s, Angle 60 degrees:
;;
(travel-distance-with-bounce 1 35 60 0)
;; ==>  59.037
(travel-distance-with-bounce 1 35 60 1)
;; ==> 81.633
(travel-distance-with-bounce 1 35 60 2)
;; ==> 88.744
(travel-distance-with-infinite-bounces 1 35 60)
;; ==> 91.508

;;
;; Note that in this model, the weak outfielder is able to throw the ball
;; a total distance of 90m (allowing for bounces).
;;

;; +++++++++++++++++++++++++
;; PROBLEM 9
;;
;; Add some better bounces.
;; +++++++++++++++++++++++++
(defn integrate-list [x0 y0 u0 v0 g m beta]
  (if (< y0 0)
    (list x0 y0 u0 v0)
    (let [dt 0.1
          speed (Math/sqrt (+ (square u0) (square v0)))]
      (let [dx (* u0 dt)
            dy (* v0 dt)
            du (* (/ -1.0 m) speed beta u0 dt)
            dv (* -1 (+ (* (/ 1.0 m) speed beta v0) g) dt)]
        (integrate-list
         (+ x0 dx)
         (+ y0 dy)
         (+ u0 du)
         (+ v0 dv)
         g m beta)))))

(defn travel-distance-list [elevation velocity angle]
  (let [rangle (degree2radian angle)]
    (let [vy (* velocity (Math/sin rangle))
          vx (* velocity (Math/cos rangle))]
      (integrate-list 0 elevation vx vy gravity mass beta))))

(defn travel-distance-with-bounce-integrated [elevation velocity angle bounces]
  (defn travel-distance-with-bounce-integrated-iter [elevation velocity angle maximum count total-distance]
    (let [bounce-list (travel-distance-list elevation velocity angle)]
      (let [new-velocity (Math/sqrt (+ (square (first (rest (rest bounce-list))))
                                       (square (first (rest (rest (rest bounce-list)))))))
            new-distance (+ (first bounce-list) total-distance)]
        (if (= count maximum)
          new-distance
          (travel-distance-with-bounce-integrated-iter
            0
            (/ new-velocity 2)
            angle
            maximum
            (+ count 1)
            new-distance)))))
  (travel-distance-with-bounce-integrated-iter elevation velocity angle bounces 0 0))

;;
;; In this case, the distance traveled does not "converge" as nicely
;; when the number of bounces increases without limit. Before, we modeled
;; the velocity as dropping by 50% with each bounce, which produces a series
;; that converges. In this case, we are not able to guarantee such convergence,
;; so we will leave the "infinite" test cases out of our calculations:
;;

;;
;; As before, we expect the answer we get for zero bounces to be the same
;; whether we are using the "travel-distance-with-bounce" procedure, or the
;; "travel-distance-withbounce-integrated" procedure.
;;
(= (travel-distance-with-bounce 1 45 45 0) (travel-distance-with-bounce-integrated 1 45 45 0))
;; ==> true
(= (travel-distance-with-bounce 1 45 30 0) (travel-distance-with-bounce-integrated 1 45 30 0))
;; ==> true
(= (travel-distance-with-bounce 1 45 60 0) (travel-distance-with-bounce-integrated 1 45 60 0))
;; ==> true

(= (travel-distance-with-bounce 1 55 45 0) (travel-distance-with-bounce-integrated 1 55 45 0))
;; ==> true
(= (travel-distance-with-bounce 1 55 30 0) (travel-distance-with-bounce-integrated 1 55 30 0))
;; ==> true
(= (travel-distance-with-bounce 1 55 60 0) (travel-distance-with-bounce-integrated 1 55 60 0))
;; ==> true

(= (travel-distance-with-bounce 1 35 45 0) (travel-distance-with-bounce-integrated 1 35 45 0))
;; ==> true
(= (travel-distance-with-bounce 1 35 30 0) (travel-distance-with-bounce-integrated 1 35 30 0))
;; ==> true
(= (travel-distance-with-bounce 1 35 60 0) (travel-distance-with-bounce-integrated 1 35 60 0))
;; ==> true


;;
;; Throwing the ball at 45 m/s:
;;
(travel-distance-with-bounce-integrated 1 45 45 0)
;; ==> 92.508
(travel-distance-with-bounce-integrated 1 45 45 1)
;; ==> 106.751
(travel-distance-with-bounce-integrated 1 45 45 2)
;; ==> 110.800

(travel-distance-with-bounce-integrated 1 45 30 0)
;; ==> 92.732
(travel-distance-with-bounce-integrated 1 45 30 1)
;; ==> 105.005
(travel-distance-with-bounce-integrated 1 45 30 2)
;; ==> 108.366

(travel-distance-with-bounce-integrated 1 45 60 0)
;; ==> 76.314
(travel-distance-with-bounce-integrated 1 45 60 1)
;; ==> 89.839
(travel-distance-with-bounce-integrated 1 45 60 2)
;; ==> 93.842

;;
;; Throwing the ball at 55 m/s:
;;
(travel-distance-with-bounce-integrated 1 55 45 0)
;; ==> 111.707
(travel-distance-with-bounce-integrated 1 55 45 1)
;; ==> 127.607
(travel-distance-with-bounce-integrated 1 55 45 2)
;; ==> 131.841

(travel-distance-with-bounce-integrated 1 55 30 0)
;; ==> 112.975
(travel-distance-with-bounce-integrated 1 55 30 1)
;; ==> 126.719
(travel-distance-with-bounce-integrated 1 55 30 2)
;; ==> 130.763

(travel-distance-with-bounce-integrated 1 55 60 0)
;; ==> 91.192
(travel-distance-with-bounce-integrated 1 55 60 1)
;; ==> 106.642
(travel-distance-with-bounce-integrated 1 55 60 2)
;; ==> 110.992

;;
;; Throwing the ball at 35 m/s:
;;
(travel-distance-with-bounce-integrated 1 35 45 0)
;; ==> 71.584
(travel-distance-with-bounce-integrated 1 35 45 1)
;; ==> 84.183
(travel-distance-with-bounce-integrated 1 35 45 2)
;; ==> 87.664

(travel-distance-with-bounce-integrated 1 35 30 0)
;; ==> 69.756
(travel-distance-with-bounce-integrated 1 35 30 1)
;; ==> 80.377
(travel-distance-with-bounce-integrated 1 35 30 2)
;; ==> 83.523

(travel-distance-with-bounce-integrated 1 35 60 0)
;; ==> 59.037
(travel-distance-with-bounce-integrated 1 35 60 1)
;; ==> 70.986
(travel-distance-with-bounce-integrated 1 35 60 2)
;; ==> 74.614

;;
;; It is worth noting that for 1 or 2 bounces, the distances traveled are
;; always shorter when integrating the velocities, than when using the
;; "naive" 50% model:
;;

;;
;; Velocity: 45/ms
;;
(< (travel-distance-with-bounce-integrated 1 45 45 1) (travel-distance-with-bounce 1 45 45 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 45 30 1) (travel-distance-with-bounce 1 45 30 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 45 60 1) (travel-distance-with-bounce 1 45 60 1))
;; ==> true

(< (travel-distance-with-bounce-integrated 1 45 45 2) (travel-distance-with-bounce 1 45 45 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 45 30 2) (travel-distance-with-bounce 1 45 30 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 45 60 2) (travel-distance-with-bounce 1 45 60 2))
;; ==> true

;;
;; Velocity: 55/ms
;;
(< (travel-distance-with-bounce-integrated 1 55 45 1) (travel-distance-with-bounce 1 55 45 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 55 30 1) (travel-distance-with-bounce 1 55 30 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 55 60 1) (travel-distance-with-bounce 1 55 60 1))
;; ==> true

(< (travel-distance-with-bounce-integrated 1 55 45 2) (travel-distance-with-bounce 1 55 45 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 55 30 2) (travel-distance-with-bounce 1 55 30 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 55 60 2) (travel-distance-with-bounce 1 55 60 2))
;; ==> true

;;
;; Velocity: 35/ms
;;
(< (travel-distance-with-bounce-integrated 1 35 45 1) (travel-distance-with-bounce 1 35 45 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 35 30 1) (travel-distance-with-bounce 1 35 30 1))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 35 60 1) (travel-distance-with-bounce 1 35 60 1))
;; ==> true

(< (travel-distance-with-bounce-integrated 1 35 45 2) (travel-distance-with-bounce 1 35 45 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 35 30 2) (travel-distance-with-bounce 1 35 30 2))
;; ==> true
(< (travel-distance-with-bounce-integrated 1 35 60 2) (travel-distance-with-bounce 1 35 60 2))
;; ==> true