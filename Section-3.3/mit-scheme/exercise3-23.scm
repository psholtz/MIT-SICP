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
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty deque" deque)
      (car (rear-ptr deque))))
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

;; Insert Mutators
(define (insert-front-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-cdr! (cdr new-pair) (front-ptr deque))
	   (set-car! (cdr (front-ptr deque)) new-pair)
	   (set-front-ptr! deque new-pair)))))
(define (insert-rear-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-car! (cdr new-pair) (rear-ptr deque))
	   (set-cdr! (cdr (rear-ptr deque)) new-pair)
	   (set-rear-ptr! deque new-pair)))))

;; Delete Mutators
(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-FRONT-DEQUE! called with an empty queue" queue))
	((eq? (front-ptr deque) (rear-ptr deque))
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '()))
	(else
	 (set-front-ptr! deque (cddr (front-ptr deque)))
	 (set-car! (cdr (front-ptr deque)) '()))))
(define (delete-rear-deque! deque)
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

