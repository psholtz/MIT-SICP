;;
;; Exercise 2.19
;;

;;
;; For reference, the "original" definitions of "cc" and "first-denomination" are given below:
;;
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else
	 (+ (cc amount
		(- kinds-of-coins 1))
	    (cc (- amount 
		   (first-denomination kinds-of-coins))
		kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;;
;; We will be re-defining these procedures below.
;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change-us amount)
  (cc amount us-coins))
(define (count-change-uk amount)
  (cc amount uk-coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values) 
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))
