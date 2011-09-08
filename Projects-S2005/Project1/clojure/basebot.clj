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
  (/ (* f 12.0) 39.6))

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
;; ==>
(travel-distance-with-bounce 1 55 45 1)
;; ==>
(travel-distance-with-bounce 1 55 45 2)
;; ==>
(travel-distance-with-infinite-bounces 1 55 45)
;; ==> 189.797

;;
;; Velocity 55 m/s, Angle 30 degrees:
;;
(travel-distance-with-bounce 1 55 30 0)
;; ==>
(travel-distance-with-bounce 1 55 30 1)
;; ==>
(travel-distance-with-bounce 1 55 30 2)
;; ==>
(travel-distance-with-infinite-bounces 1 55 30)
;; ==>

;;
;; Velocity 55 m/s, Angle 60 degrees:
;;
(travel-distance-with-bounce 1 55 60 0)
;; ==>
(travel-distance-with-bounce 1 55 60 1)
;; ==>
(travel-distance-with-bounce 1 55 60 2)
;; ==>
(travel-distance-with-infinite-bounces 1 55 60)
;; ==>

;;
;; Velocity 35 m/s, Angle 45 degrees:
;;
(travel-distance-with-bounce 1 35 45 0)
;; ==>
(travel-distance-with-bounce 1 35 45 1)
;; ==>
(travel-distance-with-bounce 1 35 45 2)
;; ==>
(travel-distance-with-infinite-bounces 1 35 45)
;; ==>

;;
;; Velocity 35 m/s, Angle 30 degrees:
;;
(travel-distance-with-bounce 1 35 30 0)
;; ==>
(travel-distance-with-bounce 1 35 30 1)
;; ==>
(travel-distance-with-bounce 1 35 30 2)
;; ==>
(travel-distance-with-infinite-bounces 1 35 30)
;; ==>

;;
;; Velocity 35 m/s, Angle 60 degrees:
;;
(travel-distance-with-bounce 1 35 60 0)
;; ==>
(travel-distance-with-bounce 1 35 60 1)
;; ==>
(travel-distance-with-bounce 1 35 60 2)
;; ==>
(travel-distance-with-infinite-bounces 1 35 60)
;; ==>

;;
;; Note that in this model, the weak outfielder is able to throw the ball
;; a total distance of 90m (allowing for bounces).
;;

;; +++++++++++++++++++++++++
;; PROBLEM 9
;;
;; Add some better bounces.
;; +++++++++++++++++++++++++ 