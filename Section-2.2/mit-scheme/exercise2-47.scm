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
;; Unit test using a frame system like the one illustrated in the text:
;;
(define origin 
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