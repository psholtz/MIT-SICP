;;
;; Exercise 2.19
;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change-us amount)
  (cc amount us-coins))
(define (count-change-uk amount)
  (cc amount uk-coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values) ) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (first-denomination coin-values)
  '())

(define (except-first-denomination coin-values) 
  '())

(define (no-more? coin-values)
  '())
