;; +++++++++++++++++++++++ 
;; PROJECT 1 
;;
;; Scheme implementation.
;; +++++++++++++++++++++++ 
(define (square x) (* x x))

(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; +++++++++++++++++++++++++++++ 
;; PROBLEM 1
;;
;; Simple kinematics equation.
;; +++++++++++++++++++++++++++++
(define position
  (lambda (a v u t)
    (+
     (* 0.5 a (square t))
     (* v t)
     u)))

;;
;; Run some unit tests:
;;
(= (position 0 0 0 0) 0)
(= (position 0 0 20 0) 20)
(= (position 0 5 10 10) 60)
(= (position 2 2 2 2) 10)
(= (position 5 5 5 5) 92.5)
(= (position 0 -10 0 1) -10)

;; ++++++++++++++++++++++++++++++ 
;; PROBLEM 2
;;
;; Implement quadratic formula.
;; ++++++++++++++++++++++++++++++
(define root1
  (lambda (a b c)
    (let ((d (discriminant a b c)))
      (if (< d 0)
	  '()
	  (/ (+ (* -1 b) (sqrt d)) (* 2 a))))))

(define root2
  (lambda (a b c)
    (let ((d (discriminant a b c)))
      (if (< d 0)
	  '()
	  (/ (- (* -1 b) (sqrt d)) (* 2 a))))))

(define discriminant
  (lambda (a b c)
    (- (square b) (* 4 a c))))

;;
;; Run some unit tests:
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
;; ==> '()

;;
;; Returns nil, since the roots to this polynomial are complex.
;;

;; ++++++++++++++++++++++++++++++++++ 
;; Problem 3
;;
;; Calculate distances (and times).
;; ++++++++++++++++++++++++++++++++++ 

;;
;; The polynomial we are attempting to solve is:
;;
;; (0.5)*a*t^2 + v*t + x0 = 0
;;
;; which is the equation of motion for a particle moving under constant acceleration a, 
;; constant velocity v and initial position x0.
;;
;; In our case, the accelertion is due to gravity (9.8) and is directed downwards (i.e.,
;; negative). The initial velocity and position (i.e., elevation) are passed in as
;; parameters to the procedure. 
;;

(define (time-to-impact vertical-velocity elevation)
  (root2 (* -0.5 gravity) vertical-velocity elevation))

(define (time-to-height vertical_velocity elevation target-elevation)
  (root2 (* -0.5 gravity) vertical-velocity (- elevation target-elevation)))

;; +++++++++++++++++++ 
;; Problem 4
;;
;; Flight distance.
;; +++++++++++++++++++ 
(define (degree2radian deg)
  (/ (* deg pi) 180.))

(define (travel-distance-simple elevation velocity angle)
  (let ((rangle (degree2radian angle)))
    (let ((vy (* velocity (sin rangle)))
	  (vx (* velocity (cos rangle))))
      (let ((t0 (time-to-impact vy elevation)))
	(let ((x0 (position 0 vx 0 t0)))
	  x0)))))

(define (meters-to-feet m)
  (/ (* m 39.6) 12))

(define (feet-to-meters f)
  (/ (* f 12) 39.6))

(define (hours-to-seconds h)
  (* h 3600))

(define (seconds-to-hours s)
  (/ s 3600))

;; ++++++++++++++++++++ 
;; Problem 5
;;
;; Best angle to hit.
;; ++++++++++++++++++++ 
(define angle-increment 1.0)

(define (radian2degree rad) (/ (* rad 180.) pi))

(define (find-best-angle velocity elevation)
  (find-best-angle-iter velocity elevation 0.0 0.0 0.0))

(define (find-best-angle-iter velocity elevation best-distance best-angle test-angle)
  (if (> test-angle 90)
      best-angle
      (let ((test-distance (travel-distance-simple elevation velocity test-angle))
	    (next-angle (+ test-angle angle-increment)))
	(if (> test-distance best-distance)
	    (find-best-angle-iter velocity elevation test-distance test-angle next-angle)
	    (find-best-angle-iter velocity elevation best-distance best-angle next-angle)))))

;; +++++++++++++++++++ 
;; Problem 6 
;; 
;; Incorporate drag.
;; +++++++++++++++++++ 
(define drag-coeff 0.5)
(define density 1.25)
(define mass 0.145)
(define diameter 0.074)
(define beta (* 0.5 drag-coeff density (* pi 0.25 (square diameter))))

(define (integrate x0 y0 u0 v0 g m beta)
  (if (< y0 0)
      x0
      (let ((dt 0.1)
	    (speed (sqrt (+ (square u0) (square v0)))))
	(let ((drag (* beta (square speed))))
	  (let ((dx (* u0 dt))
		(dy (* v0 dt))
		(du (* (/ -1 m) speed beta u0 dt))
		(dv (* -1 (+ (* (/ 1 m) speed beta v0) g) dt)))
	    (integrate (+ x0 dx)
		       (+ y0 dy)
		       (+ u0 du)
		       (+ v0 dv)
		       g m beta))))))
	    
(define (travel-distance elevation velocity angle)
  (let ((rangle (degree2radian angle)))
    (let ((vy (* velocity (sin rangle)))
	  (vx (* velocity (cos rangle))))
      (integrate 0 elevation vx vy gravity mass beta))))

;; +++++++++++++++++++++++++ 
;; Problem 7
;;
;; Aim and throw the ball.
;; +++++++++++++++++++++++++ 
(define (throw-ball elevation velocity distance)
  (throw-ball-iter elevation velocity distance 0 -90))


(define (throw-ball-iter elevation velocity distance shortest-time test-angle)
  (if (> test-angle 90)
      shortest-time
      (let ((sample-distance (travel-distance elevation velocity test-angle))
	    (radian-angle (degree2radian test-angle))
	    (next-angle (+ test-angle angle-increment))
	    (tolerance 5))
	(if (< (abs (- sample-distance distance)) tolerance)
	    (let ((vy (* velocity (sin radian-angle))))
	      (let ((t0 (time-to-impact vy elevation)))
		(cond ((or (= shortest-time 0) (< t0 shortest-time))
		       (throw-ball-iter elevation velocity distance t0 next-angle))
		      (else
		       (throw-ball-iter elevation velocity distance shortest-time next-angle)))))
	    (throw-ball-iter elevation velocity distance shortest-time next-angle)))))

;;
;; convert from miles-per-hour to meters-per-second
;;
(define (mph-to-ms mph)
  (feet-to-meters (* (seconds-to-hours mph) 5280)))

;; +++++++++++++++++++ 
;; Problem 8
;;
;; Add some bounces.
;; +++++++++++++++++++ 
(define (travel-distance-with-bounce elevation velocity angle bounces)
  (travel-distance-with-bounce-iter elevation velocity angle bounces 0 0))

(define (travel-distance-with-bounce-iter elevation velocity angle maximum count total-distance)
  (let ((bounce-distance (travel-distance elevation velocity angle)))
    (let ((new-total-distance (+ total-distance bounce-distance)))
      (if (= count maximum)
	  new-total-distance
	  (travel-distance-with-bounce-iter 0
					    (/ velocity 2)
					    angle
					    maximum
					    (+ count 1)
					    new-total-distance)))))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Problem 9
;;
;; Add some better bounces.
;; (Not sure that we're supposed to be using lists, car and 
;; cdr yet, but it seemed to be the most natural way to 
;; attack the problem). 
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
(define (travel-distance-with-bounce-integrated elevation velocity angle bounces)
  (travel-distance-with-bounce-integrated-iter elevation velocity angle bounces 0 0))

(define (travel-distance-with-bounce-integrated-iter elevation velocity angle maximum count total-distance)
  (let ((bounce-list (travel-distance-list elevation velocity angle)))
    (let ((new-velocity (sqrt (+ (square (car (cdr (cdr bounce-list))))
				 (square (car (cdr (cdr (cdr bounce-list))))))))
	  (new-distance (+ (car bounce-list) total-distance)))
      (if (= count maximum)
	  new-distance
	  (travel-distance-with-bounce-integrated-iter 0
						       (/ new-velocity 2)
						       angle
						       maximum
						       (+ count 1)
						       new-distance)))))

(define (travel-distance-list elevation velocity angle)
  (let ((rangle (degree2radian angle)))
    (let ((vy (* velocity (sin rangle)))
	  (vx (* velocity (cos rangle))))
      (integrate-list 0 elevation vx vy gravity mass beta))))

(define (integrate-list x0 y0 u0 v0 g m beta)
  (if (< y0 0)
      (list x0 y0 u0 v0)
      (let ((dt 0.1)
	    (speed (sqrt (+ (square u0) (square v0)))))
	(let ((drag (* beta (square speed))))
	  (let ((dx (* u0 dt))
		(dy (* v0 dt))
		(du (* (/ -1 m) speed beta u0 dt))
		(dv (* -1 (+ (* (/ 1 m) speed beta v0) g) dt)))
	    (integrate-list (+ x0 dx)
			    (+ y0 dy)
			    (+ u0 du)
			    (+ v0 dv)
			    g m beta))))))