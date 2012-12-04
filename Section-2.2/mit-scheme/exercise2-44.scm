;;
;; Exercise 2.44
;;
;; Define the procedure "up-split" used by "corner-split". It is similar to "right-split", except that it switches the roles 
;; of "below" and "beside".
;;

;;
;; Definition of "right-split":
;;
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;;
;; Definition of "up-split":
;;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))