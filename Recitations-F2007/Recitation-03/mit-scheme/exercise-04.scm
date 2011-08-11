;;
;; Exericse 4
;; 
;; Write an iterative procedure that computes e.
;;

;;
;; Define the "factorial" procedure:
;;
(define (fact n)
  (cond ((= n 0) 1)
	(else
	 (* n (fact (- n 1))))))

;;
;; Define an iterative version of the "e" procedure:
;;
(define (e)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (term n)
    (/ 1.0 (fact n)))
  (define (e-iter c t)
    (let ((next (+ t (term c))))
      (if (close-enough? t next)
	  next
	  (e-iter (+ c 1) next))))
  (e-iter 0 0))

;;
;; Unit test:
;;
(e)
;; ==> 2.7182815255731922

