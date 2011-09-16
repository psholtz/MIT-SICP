;;
;; Exercise 1.24
;;
;; Modify the "timed-prime-test" procedure of Exercise 1.22 to use "fast-prime?" (the Fermat method), and test 
;; each of the 12 primes you found in that exercise. Since the Fermat test has O(lg n) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1,000? Do 
;; your data bear this out? Can you explain any discrepency you find?
;;

;;
;; [WORKING]
;;

;;
;; First let's define the code that allows us to check for primes:
;;
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (square (expmod base (/ exp 2) m)) m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(t '())))

(defun even? (n) (= (% n 2) 0))

(defun square (n) (* n n))

;;
;; Let's run some unit tests, to make sure it works:
;;
(fermat-test 3)
;; ==> t

(fermat-test 4)
;; ==> nil

(fermat-test 5)
;; ==> t

(fermat-test 6)
;; ==> nil

(fermat-test 7)
;; ==> t

(setq n 100)  ;; run the test 100 times
(fast-prime? 3 n)