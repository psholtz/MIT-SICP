;;
;; Exercise 5
;;

(define (intersection line1 line2)
  (define (between x a b)
    (and (>= x a)
	 (<= x b)))
  (let ((a (line-segment-start line1))
	(b (line-segment-end line1))
	(c (line-segment-start line2))
	(d (line-segment-end line2)))
  