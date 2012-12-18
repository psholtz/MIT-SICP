

;;
;; Exercise 4
;;
;; [WORKING]
;;

;;
;; A simple definition would be:
;;
(define (dequeue q)
  (cdr q))

(define q (enqueue 4 (enqueue 5 (
