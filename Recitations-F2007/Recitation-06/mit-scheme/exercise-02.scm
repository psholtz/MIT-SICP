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

(define (same-class? c1 c2)
 (equal? (get-class-number c1) (get-class-number c2)))

;;
;; Previous solutions
;;
(define (empty-schedule) '())

;;
;; Exercise 2
;;
;; Write a selector that when given a class and a schedule, returns a new schedule
;; including the new class
;;
(define (add-class class schedule)
  (append schedule (list class)))

;;
;; Run some unit tests.
;;
(define u1 (make-units 3 3 3))
(define calc1 (make-class 101 u1))
(define calc2 (make-class 102 u2))

;;
;; Now try to build a schedule using these:
;;
(define s (add-class calc1 (empty-schedule)))
(define s (add-class calc2 s))

;;
;; Inspect the schedule:
;;
(car s)
;; ==> (101 (3 3 3))
(cadr s)
;; ==> (102 (3 3 3))

;;
;; The order of growth in both time and space is linear in the variable
;; "schedule", that is, it is O(n) where "n" is the length of the list
;; structure "schedule".
;;