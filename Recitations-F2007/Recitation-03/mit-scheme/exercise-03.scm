;;
;; Exercise 3
;;
;; Write a procedure that computes e.
;;

;; 
;; e can be expressed as a series summation where the n-th term in the series is 1/n!. 
;;
;; Using this knowledge, and the "fact" procedure we designed above, we can design 
;; a procedure that will calculate "e" to within any desired tolerance.
;;

;;
;; Definition of the "factorial" procedure:
;;
(define (fact n)
  (cond ((= n 0) 1)
	(else
	 (* n (fact (- n 1))))))

;;
;; Definition of the "e" procedure.
;;
;; This procedure generates a recursive computational process.
;;
(define (e)
  (define tolerance 0.00001)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (term n)
    (/ 1.0 (fact n)))

  '())