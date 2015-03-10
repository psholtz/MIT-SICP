;;
;; Exercise 3.28
;;
;; Define an or-gate as a primitive function box. Your "or-gate" constructor 
;; should be similar to "and-gate".
;;

;;
;; Define the or-gate:
;;
(define (or-gate i1 i2 output)
  (define (or-action-procedure)
    (let ((new-value 
	   (logical-or (get-signal i1) (get-signal i2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! i1 or-action-procedure)
  (add-action! i2 or-action-procedure)
  'ok)

;;
;; The logical-or procedure checks to make sure the two inputs are valid 
;; inputs (i.e., either 0 or 1). If the inputs are valid, it returns the 
;; logical-or of the two signals.
;;
(define (logical-or i1 i2)
  ;; define error handler
  (define (error-handler)
    (error "Invalid signal" i1 i2))

  ;; check to make sure i1, i2 are either 0 or 1
  (if (and (or (= i1 0) (= i1 1))
	   (or (= i2 0) (= i2 1)))

      ;; perform logical-or
      (cond ((or  (= i1 1) (= i2 1)) 1)
	    ((and (= i1 0) (= i2 0)) 0)
	    (else
	     (error-handler)))

      ;; input signals are wrong
      (error-handler)))

;;
;; Unit test:
;;
(logical-or 1 1)
;; ==> 1
(logical-or 1 0)
;; ==> 1
(logical-or 0 1)
;; ==> 1
(logical-or 0 0)
;; ==> 0

(logical-or 2 1)
;; ==> Invalid signal 2 1
(logical-or 1 2) 
;; ==> Invalid signal 1 2
(logical-or 2 2)
;; ==> Invalid signal 2 2

;;
;; To unit test the procedure, we need the "make-wire" procedure defined
;; in the text, along with its corresponding accessors and mutators:
;;
(define (make-wire)
  (let ((signal-value 0) 
	(action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else
	     (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))

;;
;; Accessors and mutators for "make-wire":
;;
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;;
;; Define "after-delay" procedure:
;;
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

;;
;; The supporting "agenda" data structure. 
;;
;; An agenda is made up of time segments. Each time segment is a pair 
;; consisting of a number (the time) and a queue that holds the 
;; procedures that are scheduled to be run during that time segment.
;;
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

;;
;; (desc of sorting table)
;;
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments))
		       action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr! 
	       segments
	       (cons (make-new-time-segment time action)
		     (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments!
	 agenda
	 (cons (make-new-time-segment time action)
	       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))



;;
;; Finally, the "propagate" procedure which actually runs the simulation:
;;
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))