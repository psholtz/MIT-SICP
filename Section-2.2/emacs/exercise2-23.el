;;
;; Exercise 2.23
;;
;; The procedure "for-each" is similar to "map". It takes as arguments a procedure and a list 
;; of elements. However, rather than forming a list of the results, "for-each" just applies the 
;; procedure to each of the elements in turn, from left to right. The values returned by applying
;; the procedure to the elements are not used at all -- "for-each" is used with procedures that 
;; perfom an action, such as printing. For example:
;;
;; (for-each (lambda (x) (newline) (display x))
;;           (list 57 321 88))
;;
;; 57
;; 321
;; 88
;;

;;
;; Define the procedure:
;;
(defun for-each (func items)
  (defun for-each-iter (work answer)
    (if (null work)
	'()
      (for-each-iter (cdr work)
		     (append answer
			     (list (funcall func (car work)))))))
  (for-each-iter items '()))

;;
;; Try out the new procedure:
;;
(for-each (lambda (x) (newline) (princ x))
	  (list 57 321 88))

;; ==> 57
;; ==> 321
;; ==> 88nil