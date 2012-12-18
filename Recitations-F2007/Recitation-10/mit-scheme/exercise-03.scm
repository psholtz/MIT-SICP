;;
;; Exercise 3
;;
;; [WORKING]
;;

;;
;; A simple definition would be:
;;
(define (enqueue x q)
  (append q (list x)))

(enqueue 4 (enqueue 5 (enqueue 6 (empty-queue))))
;; ==> (6 5 4)

;;
;;