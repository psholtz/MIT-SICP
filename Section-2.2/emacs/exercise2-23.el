
(defun for-each (func items)
  (defun for-each-iter (work answer)
    (if (null work)
	'()
      (for-each-iter (cdr work)
		     (append answer
			     (list (funcall func (car work)))))))
  (for-each-iter items '()))

(for-each (lambda (x) (newline) (princ x))
	  (list 57 321 88))