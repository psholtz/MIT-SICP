;;
;; [working]
;;

;;
;; Internal procedures:
;;
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

;;
;; Deque API
;;

;; Constructor
(define (make-deque)
  (cons '() '()))

;; Selectors
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty deque" deque)
      (car (rear-ptr deque))))

;; Insert Mutators
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! new-pair (front-ptr deque))
	   (set-front-ptr! deque new-pair)
	   deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque))))

;; Delete Mutators
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-FRONT-DEQUE! called with an empty queue" queue))
	((eq? (front-ptr deque) (rear-ptr deque))
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '()))
	(else
	 (set-front-ptr! deque (cddr (front-ptr deque)))
	 (set-car! (cdr (front-ptr deque)) '()))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-REAR-DEQUE! called with an empty queue" queue))
	((eq? (front-ptr deque) (rear-ptr deque))
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '()))
	(else
	 (set-rear-ptr! deque (cadr (rear-ptr deque)))
	 (set-cdr! (cdr (rear-ptr deque)) '()))))

;; Print Procedure
(define (print-deque deque)
  (define (printable-deque-iter q)
    (if (null? q)
	'()
	(cons (car q)
	      (printable-deque-iter (cddr q)))))
  (newline)
  (display (printable-deque-iter (front-ptr deque))))

