;;
;; Exercise 2.54 
;;
;; Two lists are said to be equal? if they contain equal elements arranged in the same order. For example
;;
;; (equal? '(this is a list) '(this is a list))
;;
;; is true, but
;;
;; (equal? '(this is a list) '(this (is a) list))
;;
;; is false. To be more precise, we can define "equal?" recursively in terms of the basic "eq?" equality 
;; of symbols by saying that "a" and "b" are "equal?" if they are both symbols and the symbols are "eq?", 
;; or if they are both lists such that "(car a)" is "equal?" to "(car b)" and "(cdr a)" is "equal?" to 
;; "(cdr b)". Using this idea, implement "equal?" as a procedure.
;;

;; [[WORKING]]

(define (equal? a b)
  (cond ((and (list? a) (list? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	((or (list? a) (list? b))
	 #f)
	((and (null? a) (null? b))
	 #t)
	((or (null? a) (null? b))
	 #f)
	(else (eq? a b))))

;;
;; Run the unit tests:
;;
(equal? 'a 'a)
;; ==>

(equal? 'a '())
;; ==>

(equal? 'a 'b)
;; ==>

(equal? '() '())
;; ==>

(equal? '(this is a list) '(this is a list))
;; ==>

(equal? '(this is a list) '(this (is a) list))
;; ==> #f

(equal? '(a b c) 'b)
;; ==> #f 

(equal? 1 1)
;; ==> #t

(equal? 1 2)
;; ==> #f

(equal? 1 '())
;; ==>

(equal? 1 '(this is a list))
;; ==>

(equal? '() '(this is a list))
;; ==>

(equal? 1 'a)
;; ==>