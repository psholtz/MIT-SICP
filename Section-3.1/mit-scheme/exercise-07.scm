;; [working]

;;
;; Make a password list instead of just one password
;;
(define (make-account pwd balance)
  (let ((pwd-list (list pwd)))
    ;; withdraw procedure
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  (display "Insufficient funds")))
    
    ;; "deposit" procedure
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    ;; "make-join" procedure
    (define (make-joint pwd)
      (set! pwd-list (cons pwd pwd-list))
      dispatch)

    ;; "dispatch" procedure
    (define (dispatch pwd m)
      (cond ((memq pwd pwd-list)
	     (cond ((eq? m 'withdraw) withdraw)
		   ((eq? m 'deposit) deposit)
		   ((eq? m 'make-joint) make-joint)
		   (else
		    (error "Unknown request -- ACCOUNT" m))))
	    (else
	     (lambda (x) (display "Incorrect password")))))
    dispatch))

(define (make-joint account account-pwd user-pwd)
  ((account account-pwd 'make-joint) user-pwd))

;;
;; A weakness is that either can use hte other's password.
;;
  
