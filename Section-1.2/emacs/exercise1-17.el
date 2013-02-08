;;
;; Exercise 1.17
;;
;; The exponentiation algorithms in this section are based on performing exponentiation by 
;; means of repeated multiplication. In a similar way, one can perform integer multiplication
;; by means of repeated addition. The following multiplication procedure (in which it is 
;; assumed that our language can only add, not multiply) is analogous to the "expt" procedure:
;;
;; (define (* a b)
;;  (if (= b 0)
;;      0
;;      (+ a (* a (- b 1)))))
;;
;; This algorithm takes a number of steps that is linear in b. Now suppose we include, together
;; with addition, operations "double", which double an integer, and "halve", which divides an 
;; (even) integer by 2. Using these, design a multiplication procedure analogous to "fast-expt"
;; that uses a logarithmic number of steps.
;;

(defun double (x) (+ x x))
(defun halve (x) (/ x 2))
(defun even? (x) (= (% x 2) 0))

(defun * (a b)
  (cond ((= b 0) 0)
	((even? b) (double (* a (halve b))))
	(t
	 (+ a (* a (- b 1))))))

;;
;; Unit test:
;;
(* 3 5)
;; ==> 15

;;
;; Expanding the call graph (for edification):
;;
(* 3 5)
(+ 3 (* 3 (- 5 1)))
(+ 3 (* 3 4))
(+ 3 (double (* 3 (halve 4))))
(+ 3 (double (* 3 2)))
(+ 3 (double (double (* 3 (halve 2)))))
(+ 3 (double (double (* 3 1))))
(+ 3 (double (double (+ 3 (* 3 (- 1 1))))))
(+ 3 (double (double (+ 3 (* 3 0)))))
(+ 3 (double (double (+ 3 0))))
(+ 3 (double (double 3)))
(+ 3 (double (+ 3 3)))
(+ 3 (double 6))
(+ 3 (+ 6 6))
(+ 3 12)
15