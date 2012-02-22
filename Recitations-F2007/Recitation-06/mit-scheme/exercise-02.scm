;;
;; Exercise 2
;;
;; Write a selector that when given a class and a schedule, returns a new schedule
;; including the new class
;;
(define (add-class class schedule)
  (append schedule (list class)))

;; [working -> order of growth]