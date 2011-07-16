;;
;; Exercise 2.25
;;


(define a (list 1 3 (list 5 7) 9))


(car (cdr (car (cdr (cdr a)))))

(define b (list (list 7)))

(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))