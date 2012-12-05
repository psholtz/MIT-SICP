;;
;; Exercise 2.47
;;
;; Here are two possible constructors for frames:
;;
;;  (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;;
;;  (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;;
;; For each constructor supply the appropriate selectors to produce an implementation for frames.
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
  (cons x y))
(define (xcor-vect p)
  (car p))
(define (ycor-vect p)
  (cdr p))

(define origin (make-vect -2 1))
(define edge1 (make-vect -1 2))
(define edge2 (make-vect 4 3))

;;
;; Run the unit test:
;;
(define f (make-frame origin edge1 edge2))
f
;; ==> ((-2 . 1) (-1 . 2) (4 . 3))
(origin-frame f)
;; ==> (-2 . 1)
(edge1-frame f)
;; ==> (-1 . 2)
(edge2-frame f)
;; ==> (4 . 3)

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
;; ==> ((-2 . 1) (-1 . 2) 4 . 3)

(origin-frame f)
;; ==> (-2 . 1)
(edge1-frame f)
;; ==> (-1 . 2)
(edge2-frame f)
;; ==> (4 . 3)

;;
;; Note: we will use the same data model that is used in "SICP Picture Language" for Dr. Racket
;;
;; These are the best definitions/constructors/selectors to use for that purpose:
;;
(define (make-frame origin edge1 edge2)
  (list 'frame origin edge1 edge2))

(define (origin-frame f)
  (cadr f))
(define (edge1-frame f)
  (caddr f))
(define (edge2-frame f)
  (cadddr f))