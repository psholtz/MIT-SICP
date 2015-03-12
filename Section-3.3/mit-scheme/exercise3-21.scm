;;
;; Exercise 3.21
;;
;; [working]
;;

;;
;; Import the queue module:
;;
(load "queue.scm")

;;
;; The queue itself is pointed to by the "front-ptr" of queue pair:
;;
(define (print-queue queue)
  (newline)
  (display (front-ptr queue)))
			 