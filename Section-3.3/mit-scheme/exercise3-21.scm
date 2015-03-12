;;
;; Exercise 3.21
;;
;; [working]
;;

;;
;; [explanation]
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
			 
;;
;; Instruction sequence used by Ben Bitdiddle:
;;
(define q1 (make-queue))
(print-queue q1)
;; ==> ()

(insert-queue! q1 'a)
(print-queue q1) 
;; ==> (a)

(insert-queue! q1 'b)
(print-queue q1)
;; ==> (a b)

(delete-queue! q1)
(print-queue q1)
;; ==> (b)

(delete-queue! q1)
(print-queue q1)
;; ==> ()