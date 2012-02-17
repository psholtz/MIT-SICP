;;
;; Exericse 4
;;
;; Now extend the point abstraction to handle line segments, with a constructor
;; "(make-line-segment p1 p2)" and selectors "line-segment-start" and "line-segment-end".
;;

;;
;; Define the constructor for the line segments:
;;
(defun make-list-segment (p1 p2)
  (cons p1 p2))

;;
;; Define the selectors:
;;
(defun line-segment-start (line-segment)
  (car line-segment))

(defun line-segment-end (line-segment)
  (cdr line-segment))