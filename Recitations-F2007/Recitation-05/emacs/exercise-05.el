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
    (let ((dx (- (point-x start) (point-x end)))
	  (dy (- (point-y start) (point-y end))))
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
;; ==>

(slope d2)
;; ==>

(slope d3)
;; ==>

(slope d4)
;; ==>

(slope d5)
;; ==>

(slope d6)
;; ==>