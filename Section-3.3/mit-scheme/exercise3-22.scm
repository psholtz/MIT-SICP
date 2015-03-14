;;
;; [working]
;;

;;
;; Define constructor:
;;
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    ;; empty-queue? 
    (define (empty-queue?)
      (null? front-ptr))
    ;; front-queue
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" queue)
	  (car (front-ptr))))
    ;; insert-queue!
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    ;; delete-queue!
    (define (delete-queue!)
      (cond ((empty-queue?) 
	     (error "DELETE! called with an empty queue" queue))
	    (else
	     (set! front-ptr (cdr front-ptr)))))
    ;; print-queue
    (define (print-queue)
      (newline)
      (display front-ptr))

    ;; dispatch 
    (define (dispatch m)
      (cond ((eq? 'empty-queue?) '())
	    ((eq? 'front-queue) '())
	    ((eq? 'insert-queue!) '())
	    ((eq? 'delete-queue!) '())
	    ((eq? 'print-queue) '())
	    (else
	     (error "QUEUE - Unknown Message" m))))
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
