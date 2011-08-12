;;
;; Exercise 5
;;

;;
;; First let's define some helper methods.
;;
;; It will be useful to know which point is the "left-most" (and "right-most") 
;; of a line segment (this is not always going to be the "start" or "end", respectively).
;;

;;
;; use this:
;;
(define (slope line-segment)
  (let ((start (line-segment-start line-segment))
	(end (line-segment-end line-segment)))
    (let ((dx (- (point-x start) (point-x end)))
	  (dy (- (point-y start) (point-y end))))
      (if (= dx 0)
	  '()
	  ;; use 1.0 multiplier to make it into decimal
	  (* 1.0 (/ dy dx))))))

;;
;; get "b"
;;
(define (y-intercept line-segment)
  (let ((p (line-segment-start line-segment))
	(m (slope line-segment)))
    (cond ((not (null? m))
	   (let ((x (point-x p))
		 (y (point-y p)))
	     (- y (* m x)))))))

;; 
;; check if the lines are parallel
(define (parallel? line-segment-1 line-segment-2)
  (cond ((and (null? (slope line-segment-1)) (null? (slope line-segment-2))) #t)
	((and (null? (slope line-segment-1)) (not (null? (slope line-segment-2)))) #f)
	((and (null? (slope line-segment-2)) (not (null? (slope line-segment-1)))) #f)
	(else
	 (if (= (slope line-segment-1) (slope line-segment-2))
	     #t
	     #f))))

;;
;; define the intersect method
;; (this will determine where the lines determined by the line segments 
;; will intersect);
;;
(define (intersect line-segment-1 line-segment-2)
  (if (parallel? line-segment-1 line-segment-2)
      (display "Lines are parallel!")
      (let ((m1 (slope line-segment-1))
	    (m2 (slope line-segment-2))
	    (b1 (y-intercept line-segment-1))
	    (b2 (y-intercept line-segment-2)))
	(let ((x (/ (- b2 b1) (- m1 m2))))
	  (make-point x
		      (+ (* m1 x) b1))))))
	      
;;
;; Now it still remains to determine whether the 




(define (slope line-segment)
  (let ((start (line-segment-start line-segment))
	(end (line-segment-end line-segment)))
    (let ((dx (- (point-x start) (point-x end)))
	  (dy (- (point-y start) (point-y end)))))
    (if (= dx 0)
	'()
	(/ dy dx))))


(define (slope line)
  (let ((start (line-segment-start line))
	(end (line-segment-end line)))
    (let ((dx (- (point-x start) (point-x end)))
	  (dy (- (point-y start) (point-y end)))))
    (if (= dx 0)
	'()
	(/ dy dx))))

(define (intersection line1 line2)
  (define (between x a b)
    (and (>= x a)
	 (<= x b)))
  (let ((a (line-segment-start line1))
	(b (line-segment-end line1))
	(c (line-segment-start line2))
	(d (line-segment-end line2)))
  