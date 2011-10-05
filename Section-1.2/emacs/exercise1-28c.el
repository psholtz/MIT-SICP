;;
;; Mills' prime
;;
;; Elliptic Curve Primality Proving (ECPP) is a method based on elliptic curves to prove the primality
;; of a number. ECPP is currently in practice the fastest known algorithm for testing the primality of 
;; general numbers, but the worst-case execution time is not known. 
;;
;; In 2006 the largest prime that had been proved with ECPP was the 20,562-digit Mills' prime:
;;
;; (((((((((2^3 + 3)^3 + 30)^3 + 6)^3 + 80)^3 + 12)^3 + 450)^3 + 894)^3 + 3636)^3 + 70756)^3 +97220
;;
;; Our interest in this number is in the recursive manner in which the number is defined. 
;;
;; To wit, consider the following procedure:
;;
(defun mills-iter (x v)
  (cond ((> (length x) 0)
	 (mills-iter (cdr x) (+ (cube v) (car x))))
	(t v)))

(defun cube (x) (* x x x))

;;
;; With this, we can define Mills' prime as follows:
;;
(defun mills-prime ()
  (mills-iter (list 0 2 3 30 6 80 12 450 894 3636 70756 97220) 0))
;; ==> -158410587538120865

;;
;; Doesn't work!!
;; 
;; Not surprising, Emacs is weak mathematically.
;;

;;
;; It is also worth noting that many of the steps in the iteration generate prime numbers:
;;
(setq mills-1 (list 0 2 3))
(mills-iter mills-1 0)
;; ==> 11 
(prime? 11)
;; ==> t

(setq mills-2 (list 0 2 3 30))
(mills-iter mills-2 0)
;; ==> 1361
(prime? 1361)
;; ==> t