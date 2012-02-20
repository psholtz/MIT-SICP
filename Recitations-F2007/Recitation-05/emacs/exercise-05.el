;;
;; Exercise 5
;;
;; Write a procedure "(intersection seg1 seg2)" that returns a point where two 
;; line segments intersect if they do, and returns #f if they do not intersect.
;; Be sure to honor the abstractions defined.
;;

;;
;; It will be useful to first define some helper methods.
;;
;; We would like to know both (a) the slope of the line; and (b) where the line
;; segment would intercept the y-axis if extended in both directions indefinitely 
;; (i.e., extend the line segment to a full line). 
;;
;; In other words, we would like to rewrite the line segment in terms of "y=mx+b" 
;; from standard high school algebra, and use this to solve the problem.
;;

;;
;; A procedure to find the slope of the line segment.
;;
;; Returns the numerical slope of the line when it exists.
;; If the line is vertical and the slope is "infinite", it
;; returns '(). 
;;
(defun slope (line-segment)
  (let ((start (line-segment-start line-segment))
	(end (line-segment-end line-segment)))
    (let ((dx (* 1.0 (- (point-x start) (point-x end))))
	  (dy (* 1.0 (- (point-y start) (point-y end)))))
      (if (= dx 0)
	  '()
	;; use 1.0 multiplier to make it into decimal
	(* 1.0 (/ dy dx))))))

;;
;; Run some unit tests.
;;
;; Define four points on a square:
;;
(setq p1 (make-point 1 1))
(setq p2 (make-point 1 -1))
(setq p3 (make-point -1 -1))
(setq p4 (make-point -1 1))

;;
;; Define the six line segments joining them together:
;;
(setq d1 (make-line-segment p1 p2))
(setq d2 (make-line-segment p2 p3))
(setq d3 (make-line-segment p3 p4))
(setq d4 (make-line-segment p4 p1))
(setq d5 (make-line-segment p1 p3))
(setq d6 (make-line-segment p2 p4))

;;
;; Test the slopes of these line segments:
;;
(slope d1)
;; ==> '()

(slope d2)
;; ==> 0.0

(slope d3)
;; ==> '()

(slope d4)
;; ==> 0.0

(slope d5)
;; ==> 1.0

(slope d6)
;; ==> -1.0

;;
;; So far, so good.
;;
;; Let's make sure that the slope of the line segment is the same, 
;; no matter which 'direction' we define the line segment as going in:
;;
(= (slope (make-line-segment p1 p3)) (slope (make-line-segment p3 p1)))
;; ==> t

(= (slope (make-line-segment p2 p4)) (slope (make-line-segment p4 p2)))
;; ==> t

(= (slope (make-line-segment p2 p3)) (slope (make-line-segment p3 p2)))
;; ==> t

;;
;; The test has to be defined slightly differently for "vertical" (i.e., 
;; infinite slope) line segments.
;;
(and (null (slope (make-line-segment p1 p2)))
     (null (slope (make-line-segment p2 p1))))
;; ==> t

;;
;; Finally, let's test some lines that have differing slopes
;;
(setq origin (make-point 0 0))
(setq q1 (make-point 1 2))
(setq q2 (make-point 1 3))
(setq q3 (make-point 2 1))
(setq q4 (make-point 3 1))

(slope (make-line-segment origin q1))
;; ==> 2.0
(slope (make-line-segment origin q2))
;; ==> 3.0
(slope (make-line-segment origin q3))
;; ==> 0.5
(slope (make-line-segment origin q4))
;; ==> 0.33333333333

;;
;; Next we want to determine the y-intercept of the line segment, 
;; which would give us the "b" in the y=mx+b formulation.
;;
(defun y-intercept (line-segment)
  (let ((p (line-segment-start line-segment))
	(m (slope line-segment)))
    (cond ((not (null m))
	   (let ((x (point-x p))
		 (y (point-y p)))
	     (- y (* m x)))))))

;;
;; Run some unit tests:
;;
(y-intercept d1)
;; ==> nil
(y-intercept d2)
;; ==> -1.0
(y-intercept d3)
;; ==> nil
(y-intercept d4)
;; ==> 1.0
(y-intercept d5)
;; ==> 0.0
(y-intercept d6)
;; ==> 0.0

;; 
;; If the lines are parallel, they won't intersect! 
;;
;; It will be useful to check for this, so let's define it:
;;
(defun parallel? (line-segment-1 line-segment-2)
  (cond ((and (null (slope line-segment-1)) (null (slope line-segment-2))) t)
	((and (null (slope line-segment-1)) (not (null (slope line-segment-2)))) '())
	((and (null (slope line-segment-2)) (not (null (slope line-segment-1)))) '())
	(t
	 (if (= (slope line-segment-1) (slope line-segment-2))
	     t
	   '()))))

;;
;; Run the unit tests:
;;
(parallel? d1 d1)
;; ==> nil
(parallel? d1 d3)
;; ==> t
(parallel? d1 d4)
;; ==> nil
(parallel? d1 d5)
;; ==> nil
(parallel? d1 d6)
;; ==> nil

(parallel? d2 d3)
;; ==> nil
(parallel? d2 d4)
;; ==> t
(parallel? d2 d5)
;; ==> nil
(parallel? d2 d6)
;; ==> nil

(parallel? d3 d4)
;; ==> nil
(parallel? d3 d5)
;; ==> nil
(parallel? d3 d6)
;; ==> nil

(parallel? d4 d5)
;; ==> nil
(parallel? d4 d6)
;; ==> nil

(parallel? d5 d6)
;; ==> nil

(parallel? d1 d1)
;; ==> t
(parallel? d2 d2)
;; ==> t
(parallel? d3 d3)
;; ==> t
(parallel? d4 d4)
;; ==> t
(parallel? d5 d5)
;; ==> t
(parallel? d6 d6)
;; ==> t

;; 
;; We're getting close to the final solution.
;; 
;; Next let's define a procedure called "intersect" - it will take two 
;; line segments as arguments, and determine the point at which the 
;; corresponding lines intersect (if they do intersect), or else indicate 
;; to the user that the lines are parallel (and hence do not intersect).
;;
(defun intersect (line-segment-1 line-segment-2)
  (if (parallel? line-segment-1 line-segment-2)
      (display "The lines are parallel!")
    (let ((m1 (slope line-segment-1))
	  (m2 (slope line-segment-2))
	  (b1 (y-intercept line-segment-1))
	  (b2 (y-intercept line-segment-2)))
      (cond ((null? m1)
	     (let ((x (point-x (line-segment-start line-segment-1))))
	       (make-point x (+ (* m2 x) b2))))
	    ((null? m2)
	     (let ((x (point-x (line-segment-start line-segment-2))))
	       (make-point x (+ (* m1 x) b1))))
	    (t
	     (let ((x (/ (- b2 b1) (- m1 m2))))
	       (make-point x 
			   (+ (* m1 x) b1))))))))

;;
;; Run the unit tests:
;;
(intersect d1 d1)
