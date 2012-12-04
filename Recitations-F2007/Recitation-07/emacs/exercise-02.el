;;
;; Define "length" using a higher order list predicate.
;;

;;
;; Presently "length" is defined as follows:
;;
(defun length (lst)
  (if (null lst)
      0
    (+ 1 (length (cdr lst)))))

;;
;; We can define "length" in terms of "fold-right" (or "accumulate") as follows:
;;


(defun length (lst)
  (fold-right #'(lambda (a b) (+ b 1)) 0 lst))
