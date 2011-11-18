;;
;; Exercise 2.19
;;

;;
;; For reference, the "original" definitions of "cc" and "first-denomination" are given below:
;;
(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(t
	 (+ (cc amount
		(- kinds-of-coins 1))
	    (cc (- amount
		   (first-denomination kinds-of-coins))
		kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;;
;; We will be re-defining these procedures below.
;;
;; Copying in the code from the problem statement:
;;
(setq us-coins (list 50 25 10 5 1))
(setq uk-coins (list 100 50 20 10 5 2 1 0.5))

(defun count-change-us (amount)
  (cc amount us-coins))
(defun count-change-uk (amount)
  (cc amount uk-coins))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(t
	 (+ (cc amount
		(except-final-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

;;
;; Supplying the answers required to support the new computational model:
;;
(defun first-denomination (coin-values)
  (car coin-values))
(defun except-final-denomination (coin-values)
  (cdr coin-values))
(defun no-more? (coin-values)
  (null coin-values))

;;
;; Let's run these results and see what we obtain:
;;
(cc 11 us-coins)
;; ==> 4
(cc 11 uk-coins)
;; ==> 62

;;
;; (emacs is not powerful enough to calculate change for numbers ~100, as in the other examples)
;;

;;
;; Or, expressing it in terms of the procedures defined above:
;;
(count-change-us 11)
(count-change-uk 11)

;;
;; Reversing the order of the coins in the list does not affect the value obtained:
;;
(setq us-coins-reverse (reverse us-coins))
(setq uk-coins-reverse (reverse uk-coins))

(cc 11 us-coins-reverse)
;; ==> 4
(cc 11 uk-coins-reverse)
;; ==> 62

(equal (cc 11 us-coins) (cc 11 us-coins-reverse))
;; ==> t
(equal (cc 11 uk-coins) (cc 11 uk-coins-reverse))
;; ==> t

;;
;; This makes "logical" sense, since if we are counting the number of ways to divide up 
;; e.g., 11 cents using a bag of (50,25,10,5,1) denominations, the way in which the elements
;; of the bag are ordered should not make a difference.. i.e., it should not make a difference
;; whether I start counting with "1"s or with "50"s.. in either case, I should arrive at the 
;; same total number of countings given a set of elements in the bag.
;;

;;
;; I won't expand the call graph here as I did for the MIT Scheme example.
;;