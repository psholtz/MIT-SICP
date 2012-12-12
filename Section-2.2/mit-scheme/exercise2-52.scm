;;
;; Exercise 2.52
;;
;; Make changes to the square limit of "wave" shown in Figure 2.9 by working at each of the levels
;; described above. In particular:
;;
;;  (a) Add some segments to the primitive "wave" painter of Exercise 2.49 (to add a smile, for example).
;;  (b) Change the pattern constructed by "corner-split" (for example, by using only one copy of the 
;;      "up-split" and "right-split" images instead of two).
;;  (c) Modify the version of "square-limit" that uses "square-of-four" so as to assemble the corners
;;      in a different pattern (for example, you might make the big Mr. Rogers look outward frmo each 
;;      corner of the square).
;;

;; (a) Add some segments to the primitive wave painter of exercise 2.49 (add a smile, for example)

;;
;; Let's revisit the information we used to construct the original wave painter:
;;
(define (make-vect x y)
  (cons x y))
(define (xcor-vect p)
  (car p))
(define (ycor-vect p)
  (cdr p))

(define (add-vect a b)
  (make-vect 
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))))
(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))))
(define (scale-vect c a)
  (make-vect
   (* c (xcor-vect a))
   (* c (ycor-vect a))))

;; draw the "wave" man:
(define points
  '((0.4 0.0)
    (0.5 0.33)
    (0.6 0.0)
    (0.75 0.0)
    (0.6 0.45)
    (0.99 0.15)
    (0.99 0.35)
    (0.8 0.65)
    (0.6 0.65)
    (0.65 0.8)
    (0.6 0.99)
    (0.4 0.99)
    (0.35 0.8)
    (0.4 0.65)
    (0.33 0.65)
    (0.1 0.6)
    (0.0 0.8)
    (0.0 0.6)
    (0.1 0.4)
    (0.3 0.6)
    (0.33 0.5)
    (0.25 0.0)))

;; map the "make-vect" procedure over these points:
(define (make-vectors points)
  (map (lambda (p)
	 (let ((x (car p))
	       (y (cadr p)))
	   (make-vect x y)))
       points))

;; create a way to generate segments from these vectors:
(define (make-segments vectors)
  (define (make-segments-iter working total)
    (if (null? (cdr working))
	(append total (list (make-segment (car working) (car (car total)))))
	(let ((one (car working))
	      (two (cadr working)))
	  (make-segments-iter (cdr working) (append total (list (make-segment one two)))))))
  (make-segments-iter vectors '()))

(define vectors (make-vectors points))
(define segments (make-segments vectors))

;; now define the "smile":
(define p1 (make-vect 0.45 0.75))
(define p2 (make-vect 0.5 0.7))
(define p3 (make-vect 0.55 0.75))

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))

(define segments (append segments (list s1)))
(define segments (append segments (list s2)))

(define wave (segments->painter segments)
(paint wave)

;; (b) Change the pattern constructed by "corner-split" (for example, by using only one copy of the "up-split" and "right-split" images instead of two)

;;
;; The definitions of "right-split" and "up-split" are given as:
;;
(define (right-split painter n)
  (if (= n 0)
      painter 
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;
;; This is the definition of "corner-split" given in the text:
;;
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

;;
;; We can change the definition of "corner-split" as suggested in the text:
;;
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left up)
	      (bottom-right right)
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

;; (c) Modify the version of "square-limit" that uses "square-of-four" so as to assemble the corners in a different pattern (for example, you might make the big Mr. Rogers look outward from each corner of the square).

;;
;; The definition of "square-limit" (using "square-of-four") as given in the text is as follows:
;;
(define (identity x) x)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;
;; To reverse the way that Einstein "looks" in the picture, we reveres the order in which
;; the "square-of-four" procedures are applied:
;;
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180 identity flip-horiz)))
    (combine4 (corner-split painter n))))

;;
;; Pictures for all these exercises are given in the accompanying .md file.
;;