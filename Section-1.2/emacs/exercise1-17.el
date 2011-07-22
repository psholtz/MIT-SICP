;;
;; Exercise 1.17
;;

(defun double (x) (+ x x))
(defun halve (x) (/ x 2))
(defun even? (x) (= (% x 2) 0))

(defun * (a b)
  (cond ((= b 0) 0)
	((even? b) (double (* a (halve b))))
	(t
	 (+ a (* a (- b 1))))))