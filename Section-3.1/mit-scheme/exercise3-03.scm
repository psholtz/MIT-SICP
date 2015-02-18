;;
;; Exercise 3.3
;;
;; Modify the "make-account" procedure so that it creates password-protected 
;; accounts. That is, "make-account" should take a symbol as an additional 
;; argument, as in 
;;
;; (define acc (make-account 100 'secret-password))
;;
;; The resulting account object should procefss a request only if it is 
;; accompanied by the password with which the account was created, and should 
;; otherwise return a complaint:
;;
;; ((acc 'secret-password 'withdraw) 40)
;; 60
;;
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password")
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

    ;; "dispatch" procedure
    (define (dispatch pwd m)
      (cond ((eq? pwd password)
	     (cond ((eq? m 'withdraw) withdraw)
		   ((eq? m 'deposit) deposit)
		   (else
		    (error "Unknown request -- ACCOUNT" m))))
	    (else
	     (lambda (x) (display "Incorrect password")))))
    dispatch))

;;
;; Run the unit tests:
;;
(define account (make-account 'secret 1000))

((account 'secret 'withdraw) 100)
;; ==> 900

((account 'wrong 'withdraw) 200)
;; ==> "Incorrect password"

((account 'secret 'withdraw) 10000)
;; ==> "Insufficient funds"

((account 'secret 'deposit) 300)
;; ==> 1200