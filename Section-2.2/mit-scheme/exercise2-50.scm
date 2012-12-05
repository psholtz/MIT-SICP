;; 
;; Exercise 2.50
;;
;; Define the transformation "flip-horiz" which flips painters horizontally, and transformations
;; that rotate painters counterclockwise by 180 degrees and 270 degrees.
;;

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)   ;; new origin
		     (make-vect 0.0 1.0)   ;; new end of edge 1
		     (make-vect 1.0 0.0))) ;; new end of edge 2

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2
 
;;
;; To get these to work on the "SICP Picture Language" for Dr. Racket, I use the 
;; following procedures. "Picture Language" already defines a "transform-painter"
;; procedure, and this procedure uses a different call signature (i.e., seems to 
;; be incompatible with) from that defined here in the SICP text. "Picture Language"
;; also already defines a procedure calls "flip-horiz" (which seems to work just fine), 
;; and procedures "rotate180" and "rotate270".
;;
;; For this reason, to test these procedures in Dr. Racket, I'm defining the procedures
;; under slightly different names:
;;
(define (transform-painter-1 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz-1 painter)
  (transform-painter-1 painter
		       (make-vect 1.0 0.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

(define (rotate180-1 painter)
  (transform-painter-1 painter
		       (make-vect 1.0 1.0)
		       (make-vect 0.0 1.0)
		       (make-vect 1.0 0.0)))

(define (rotate270-1 painter)
  (transform-painter-1 painter
		       (make-vect 0.0 1.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))
