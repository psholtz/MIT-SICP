;;
;; Exericse 4
;;
;; Now extend the point abstraction to handle line segments, with a constructor
;; "(make-line-segment p1 p2)" and selectors "line-segment-start" and "line-segment-end".
;;

;;
;; Define the constructor for the line segments:
;;
(define (make-line-segment p1 p2)
  (cons p1 p2))

;;
;; Define the selectors:
;;
(define (line-segment-start line-segment)
  (car line-segment))

(define (line-segment-end line-segment)
  (cdr line-segment))


