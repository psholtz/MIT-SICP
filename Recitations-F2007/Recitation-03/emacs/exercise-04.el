;;
;; Exericse 4
;; 
;; Write an iterative procedure that computes e.
;;

;;
;; Define the "factorial" procedure:
;;
(defun fact (n)
  (cond ((= n 0) 1)
	(t
	 (* n (fact (- n 1))))))

;;
;; Define an iterative version of the "e" procedure:
;;
(defun e ()
  (setq tolerance 0.00001)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun term (n)
    (/ 1.0 (fact n)))
  (defun e-iter (c v)
    (let ((next (+ v (term c))))
      (if (close-enough? v next)
	  next
	(e-iter (+ c 1) next))))
  (e-iter 0 0))

;;
;; Unit test:
;;
(e)
;; ==> 2.7182815255731922