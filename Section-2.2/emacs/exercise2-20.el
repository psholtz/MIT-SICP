;;
;; Exercise 2.20
;;

(defun* same-parity (x &rest z)
  ;; define the parity-testing procedures
  (defun even? (n) (= (% n 2) 0))
  (defun odd? (n) (not (even? n)))

  ;; define the list-filtering routine
  (defun filter (func)
    (defun filter-iter (list-in list-out)
      (cond ((null list-in) list-out)
	    (t
	     (let ((a (car list-in)))
	       (if (funcall func a)
		   (filter-iter (cdr list-in) (append list-out (list a)))
		 (filter-iter (cdr list-in) list-out))))))
    (filter-iter z (list x)))

  ;; invoke the "filter" procedure appropriately
  (cond ((even? x) (filter #'even?))
	(t (filter #'odd?))))

;;
;; Test the procedure:
;;
(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

  