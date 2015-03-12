;;
;; Exercise 3.21
;;
;; Ben Bitdiddle decides to test the queue implementation described
;; above. He types in the procedure to the Lisp interpreter and proceeds
;; to try them out:
;;
;; (define q1 (make-queue))
;; (insert-queue! q1 'a)
;; ((a) a)
;; (insert-queue! q1 'b)
;; ((a b) b)
;; (delete-queue! q1)
;; ((b) b)
;; (delete-queue! q1)
;; (() b)
;;
;; "It's all wrong!" he complains. "The interpreter's response shows that
;; the last item is inserted into the queue twice. And when I delete both
;; items, the second 'b' is still there, so the queue isn't empty, even 
;; though it's supposed to be." Eva Lu Ator suggests that Ben has mis-
;; understood what is happening. "It's not that the items are going into
;; the queue twice," she explains. "It's just that the standard Lisp printer
;; doesn't know how to make sense of the queue representation. If you want 
;; to see the queue printed correctly, you'll have to define your own print
;; procedures for the queues." Explain what Eva Lu Ator is talking about. 
;; In particular, show why Ben's examples produce the printed results that
;; they do. Define a procedure "print-queue" that takes a queue as input 
;; and prints the sequence of items in the queue.
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