;;
;; Exercise 3.7
;;
;; Consider the bank account objects created by "make-account", with the 
;; password modification described in exercise 3.3. Suppose that our banking
;; system requires the ability to make joint accounts. Define a procedure 
;; "make-joint" that accomplishes this. "make-joint" should take three 
;; arguments. The first is a password-protected account. The second argument
;; must match the password with which the account was defined in order for 
;; the "make-joint" operation to proceed. The third argument is a new password.
;; "make-joint" is to create an additional access to the original account 
;; using the new password. For example, if "peter-acc" is a bank account with
;; password "open-sesame", then
;;
;;  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;;
;; will allow one to make transactions on "peter-acc" using the name "paul-acc"
;; and the password "rosebud". You may wish to modify your solution to 
;; exercise 3.3 to accommodate this new feature.
;;

;;
;; We make the following modifications to the original "make-account" procedure:
;;
;;  (1) Use a list of passwords rather than just a single password;
;;  (2) Add a definition for an internal "make-joint" procedure;
;;  (3) Add a selector for "make-joint" to the dispatch procedure;
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

;;
;; With these modifications to "make-account", we can define "make-joint" as:
;;
(define (make-joint account account-pwd user-pwd)
  ((account account-pwd 'make-joint) user-pwd))

;; 
;; Peter creates and uses his account:
;;
(define peter-acc (make-account 'open-sesame 1000))

((peter-acc 'wrong 'withdraw) 100)
;; ==> Incorrect password
((peter-acc 'open-sesame 'withdraw) 100)
;; ==> 900

;;
;; Paul can now create this joint account with Peter:
;;
(define paul-acc (make-joint peter-acc 'wrong 'rosebud))
;; ==> Incorrect password
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'deposit) 300)
;; ==> 1200

;;
;; One weakness of this model is that Paul can use Peter's password to
;; access the account, if we knows it. This is the because there is no 
;; notion of a "user" or a "user/password" combination in this model; rather
;; the only state maintained in the procedure is the password list and 
;; so long as the supplied password is on the list, it unlocks the account.
;;
((paul-acc 'open-sesame 'withdraw) 50)
;; ==> 1150