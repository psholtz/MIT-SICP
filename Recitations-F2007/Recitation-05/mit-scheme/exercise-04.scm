;;
;; Exericse 4
;;

(define (make-line-segment p1 p2)
  (cons p1 p2))

(define (line-segment-start line)
  (car line))

(define (line-segment-end line)
  (cdr line))


