;;
;; [working]
;;

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    
    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an emtpy queue" front-ptr)
	  (car (front-ptr))))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr))))

    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue" front-ptr))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))

    (define (dispatch m)
      (cond  ((eq? m 'empty-queue?)
	      (empty-queue?))
	     ((eq? m 'front-queue)
	      (front-queue))
	     ((eq? m 'insert-queue!)
	      '())
	     ((eq? m 'delete-queue!)
	      '())
	     (else
	      (error "Unknown message" message))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))