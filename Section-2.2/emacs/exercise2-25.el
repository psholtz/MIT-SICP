;;
;; Exercise 2.25
;;
;; Give combinations of "car"s and "cdr"s that will pick 7 from each of the following lists:
;;

;;
;; Question (a)
;;
(setq a (list 1 3 (list 5 7) 9))

;;
;; Solution (a)
;;
(car (cdr (car (cdr (cdr a)))))

;;
;; Question (b)
;;
(setq b (list (list 7)))

;;
;; Solution (b)
;;
(car (car b))

;;
;; Question (c)
;;
(setq c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;;
;; Solution (c)
;
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))