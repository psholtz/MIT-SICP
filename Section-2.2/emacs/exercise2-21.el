;;
;; Exercise 2.21
;;
;; The procedure "square-list" takes a list of numbers as arguments and returns a list of the squares
;; of those numbers.
;;
;; (square-list (list 1 2 3 4 5))
;; (1 4 9 16 25)
;;
;; Here are two different definitions of "square-list". Complete both of them by filling in the missing 
;; expressions:
;;
;; (define (square-list items)
;;  (if (null? items)
;;      nil
;;      (cons <??> <??>)))
;;
;; (define (square-list items)
;;  (map <??> <??>))
;;

;;
;; Define the "square" form:
;;
(defun square (x) (* x x))

;;
;; Define the first "square-list" procedure:
;;
(defun square-list (items)
  (if (null items)
      '()
    (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4 5))
;; ==> (1 4 9 16 25)

;;
;; Define the second "square-list" procedure:
;;
(defun square-list (items)
  (mapcar #'square items))

(square-list (list 1 2 3 4 5))
;; ==> (1 4 9 16 25)