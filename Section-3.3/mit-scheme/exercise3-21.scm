;;
;; Exercise 3.21
;;
;; [working]
;;

;;
;; Eva is correct. The data structure we're using to represent "queues" 
;; is actually a pair. A pointer to the  "queue" itself is stored in the 
;; first element (or 'car') of the pair, while a pointer to the last 
;; element of the queue is stored in the second element (or 'cdr') of the 
;; pair.
;;
;; To represent the queue in a manner that Ben expects, we must write a 
;; procedure that extracts and displays the first element of the pair 
;; representing the queue.
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