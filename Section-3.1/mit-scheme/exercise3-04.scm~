;;
;; Exercise 3.4
;;
;; Modify the "make-account" procedure of exercise 3.3 by adding another local state 
;; variable so that, if an accout is accessed more than seven consecutive times with 
;; an incorrect password, it invokes the procedure "call-the-cops"
;;

;;
;; Define the procedure:
;;
(define (make-account pwd balance)
  (let ((password pwd)
	(count 0))
    (define (withdraw amount)
      (if (>= balance amount)
	    (begin (set! balance (- balance amount))
		    balance)
	      (display "Insufficient funds")))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops x)
      (display "Call the cops!!"))
    (define (dispatch pwd m)
      (cond ((eq? pwd password)
	     (set! count 0)
	     (cond ((eq? m 'withdraw) withdraw)
		   ((eq? m 'deposit) deposit)
		   (else
		    (error "Unknown request -- ACCOUNT" m))))
	    (else
	     (set! count (+ count 1))
	     (if (> count 7)
		 call-the-cops
		 (lambda (x) (display "Incorrect password"))))))
    dispatch))