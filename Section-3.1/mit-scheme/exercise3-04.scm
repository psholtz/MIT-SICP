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
  (let ((password pwd))
    ;; "withdraw" procedure
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  (display "Insufficient funds")))
    
    ;; "deposit" procedure
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    ;; "call-the-cops" procedure
    (define (call-the-cops)
      (display "Call the cops!"))

    ;; "dispatch" procedure
    (define (dispatch pwd m)
      (cond ((eq? pwd password)
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

;;
;; Run the unit tests:
;;
(define account (make-account 'secret 1000))

((account 'secret 'withdraw) 100)
;; ==> 900 

((account 'secret 'deposit) 300)
;; ==> 1200

((account 'wrong 'withdraw) 100)
;; ==> Incorrect password
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Incorrect password 
((account 'wrong 'withdraw) 100)
;; ==> Call the cops!!       