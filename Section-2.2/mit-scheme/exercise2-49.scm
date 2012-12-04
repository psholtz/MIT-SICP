;;
;; Exercise 2.49
;;
;; {WORKING]
;;

;;
;; The "frame-coord-map" procedure is given as:
;;
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;;
;; The "segments->painter" procedure is given as:
;;
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each 
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))