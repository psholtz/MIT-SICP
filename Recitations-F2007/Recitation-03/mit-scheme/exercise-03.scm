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

  ;; n-th term of the summation
  (define (term n)
    (/ 1.0 (fact n)))

  ;; compute the total summation, up to n-terms
  (define (e-sum n)
    (define (e-sum-iter c)
      (if (= c n)
	  (term n)
	  (+ (term c) (e-sum-iter (+ c 1)))))
    (e-sum-iter 0))

  ;; approximate e by counting up to 10 terms
  (e-sum 10))

;;
;; Run the unit test:
;;
(e)
;; ==> 2.7182818011463845