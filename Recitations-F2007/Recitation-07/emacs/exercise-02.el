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
(defun fold-right (op init lst)
  (if (null lst)
      init
    (funcall op (car lst)
	          (fold-right op init (cdr lst)))))

(defun length (lst)
  (fold-right #'(lambda (a b) (+ b 1)) 0 lst))

;;
;; Run some unit tests:
;;
(setq x '(1 1 2 1 3))
(length x)
;; ==> 5

(setq y '(1 2 3 4 5 6 7))
(length y)
;; ==> 7
