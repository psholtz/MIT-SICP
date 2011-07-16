;;
;; Exercise 1.16
;;

(defun fast-expt (b n)
  (fast-expt-iter b n 1))

(defun fast-expt-iter (b n a)
  (cond ((= n 0) a)
	((even? n) (square (fast-expt-iter b (/ n 2) a)))
	(t
	 (fast-expt-iter b (- n 1) (* a b)))))

(defun even? (n) (= (% n 2) 0))
(defun square (n) (* n n))
