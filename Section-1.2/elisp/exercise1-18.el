;;
;; Exercise 1.18
;;

(defun double (x) (+ x x))
(defun halve (x) (/ x 2))
(defun even? (n) (= (% n 2) 0))

(defun * (a b)
  (*-iter a b 0))

(defun *-iter (a b n)
  (cond ((= a 0) n)
	((even? a) (*-iter (halve a) (double b) n))
	(t
	 (*-iter (+ a -1) b (+ b n)))))