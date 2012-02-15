;;
;; Exercise 2
;;

;; ++++++++++++++++ 
;; Core Definitions
;; ++++++++++++++++ 
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
  (= (get-class-number c1) (get-class-number c2)))

;;
;; Write a selector that when given a class and a schedule, returns a new schedule
;; including the new class
;;
(define (add-class class schedule)
  (append schedule (list class)))

;; [working -> order of growth]