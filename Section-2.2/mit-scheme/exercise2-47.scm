;;
;; Exercise 2.47
;;
;; [WORKING]
;;

;;
;; First constructor and supporting selectors:
;;
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

;;
;; Unit test using a frame system like the one illustrated in the text.
;;
;; First we need the supporting vector operations from Exercise 2.46:
;;
(define (make-vect x y)
  (list x y))
(define (xcor-vect p)
  (car p))
(define (ycor-vect p)
  (cadr p))

(define origin (make-vect -2 1))
(define edge1 (make-vect -1 2))
(define edge2 (make-vect 4 3))

;;
;; Run the unit test:
;;
(define f (make-frame origin edge1 edge2))
f
;; ==> ((-2 1) (-1 2) (4 3))
(origin-frame f)
;; ==> (-2 1)
(edge1-frame f)
;; ==> (-1 2)
(edge2-frame f)
;; ==> (4 3)

;;
;; Second constructor and supporting selectors:
;;
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cddr f))

;;
;; Again, run the unit tests:
;;
(define f (make-frame origin edge1 edge2))
f
;; ==> ((-2 1) (-1 2) 4 3)

(origin-frame f)
;; ==> (-2 1)
(edge1-frame f)
;; ==> (-1 2)
(edge2-frame f)
;; ==> (4 3)