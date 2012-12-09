;;
;; Exercise 2.53
;;
;; What would the interpreter print in response to evaluating each of the following expressions:
;;

(list 'a 'b 'c)
;; ==> (a b c)

(list (list 'george))
;; ==> ((george))

(cdr '((x1 x2) (y1 y2)))
;; ==> ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; ==> (y1 y2)

(listp (car '(a short list)))
;; ==> nil

(memq 'red '((red shoes) (blue socks)))
;; ==> nil

(memq 'red '(red shoes blue socks))
;; ==> (red shoes blue socks)