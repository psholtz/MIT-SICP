;;
;; Exercise 1.33
;;
;; You can obtain an even more general version of "accumulate" (exercise 1.32) by introducing the notion 
;; of a filter on the terms to be combined. That is, combine only those terms derived from values in the 
;; range that satisfy a specified condition. The resulting "filtered-accumulate" abstraction takes the 
;; same arguments as accumulate, together with an additional predicate of one argument that specifies
;; the filter. Write "filtered-accumulate" as a procedure. Show how to express the following using 
;; "filtered-accumulate":
;;
;; (a) the sum of the squares of the prime numbers in the interval a to b.
;;
;; (b) the product of all the positive integers less than n that are relatively prime to n.
;;

;;
;; First, define the "filtered-accumulate" procedure:
;;
(defun filtered-accumulate (combiner null-value term a next b predicate)
  (if (> a b)
      null-value
    (funcall combiner (if (funcall predicate a)
			  (funcall term a)
			null-value)
	     (filtered-accumulate combiner null-value term (funcall next a) next b predicate))))

;;
;; Define the prime predicate (take it from Exercise 1.28, which is the fastest prime testing procedure we developed):
;;
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (square (expmod base (/ exp 2) m)) m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

(defun prime? (n)
  (defun get-random-a ()
    (+ 2 (random (- n 4))))
  (defun test (a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 1) '())
	((= n 2) t)
	((= n 3) t)
	((= n 4) '())
	((= n 5) t)
	(t
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))
;;
;; Define other supporting procedures:
;;
(defun inc (n) (+ n 1))
(defun square (n) (* n n))
(defun even? (n) (= (% n 2) 0))

;;
;; Do the "prime" test
(defun sum-of-prime-squares (a b)
  (filtered-accumulate #'+ 0 #'square a #'inc b #'prime?))

;;
;; RUN SOME TESTS OF SUM OF SQUARES OF PRIMES
;;
(sum-of-prime-squares 1 1)
;; ==> 0

(sum-of-prime-squares 1 2)
;; ==> 4

(sum-of-prime-squares 1 3)
;; ==> 13

(sum-of-prime-squares 1 4)
;; ==> 13

(sum-of-prime-squares 1 5)
;; ==> 38

;;
;; We know that 1000999 is prime
;;
(setq test-value 1000999)
(= (square test-value) (sum-of-prime-squares test-value (+ test-value 1)))
;; ==> t

;; [WORKING]