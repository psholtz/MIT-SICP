;;
;; Exercise 4
;;

(define x (list 1 2 3 4 5 6 7))

;;
;; (a) (1 4 9 16 25 36 49)
;;
(map square x)

;;
;; (b) (1 3 5 7)
;;
(filter odd? x)

;;
;; (c) ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))
;;

;;
;; One simple possibility is the following:
;;
(map (lambda (y) (list y y)) x)

;;
;; or the even more pathological:
;;
(map (lambda (x) (list x x)) x)

;;
;; However, these solutions do not utilize map, filter and/or fold-right.
;;


