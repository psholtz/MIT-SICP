;;
;; [working]
;;

;;
;; Define constructor:
;;
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    ;; Internal Procedures
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" queue)
	  (car (front-ptr))))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?) 
	     (error "DELETE! called with an empty queue" queue))
	    (else
	     (set! front-ptr (cdr front-ptr)))))
    (define (print-queue)
      (newline)
      (display front-ptr))

    ;; Dispatch Procedure
    (define (dispatch m)
      (cond ((eq? 'empty-queue?) (empty-queue?))
	    ((eq? 'front-queue) (front-queue))
	    ((eq? 'insert-queue!) insert-queue!)
	    ((eq? 'delete-queue!) (delete-queue!))
	    ((eq? 'print-queue) (print-queue))
	    (else
	     (error "QUEUE - Unknown message" m))))
    dispatch))

;;
;; Define selectors and mutators:
;;
(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  (queue 'delete-queue!))
(define (print-queue queue)
  (queue 'print-queue))

;;
;; [working]
;;
