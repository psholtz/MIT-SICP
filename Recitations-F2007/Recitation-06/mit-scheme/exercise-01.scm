;;
;; Working definitions
;;
(define (make-units C L H)
 (list C L H))
(define get-units-C car)
(define get-units-L cadr)
(define get-units-H caddr)

(define (make-class number units)
 (list number units))
(define get-class-number car)
(define get-class-units cadr)

(define (get-class-total-units class)
 (let ((units (get-class-units class)))
  (+ 
   (get-units-C units)
   (get-units-L units)
   (get-units-H units))))

;;
;; Change the definition of this procedure, so we can use
;; non-integer class "numbers" as well (i.e., CALC101 for 
;; an example calculus class).
;;
;; Handout uses "=" for comparison, we will use "equal?".
;;
(define (same-class? c1 c2)
 (equal? (get-class-number c1) (get-class-number c2)))

;;
;; Exercise 1
;;
;; Define a constructor "empty-schedule" that returns an empty schedule
;;
(define (empty-schedule) '())

;; 
;; The order of growth is constant in both time and space.
;;