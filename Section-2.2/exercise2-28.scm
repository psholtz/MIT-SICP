;;
;; Exercise 2.28
;;

(define (fringe x)
  (define (fringe-iter y)
    (display (length y))
    (newline)
    (display (length (car y)))
    (newline)
    (display "===="))
  (fringe-iter x))

