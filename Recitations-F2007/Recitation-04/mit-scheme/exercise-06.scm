;;
;; Exercise 6
;;
;; Consider the recursive definition of factorial we've seen before:
;;
;; (define (fact n)
;;  (if (= n 0)
;;      1
;;      (* n (fact (- n 1)))))
;;
;; Running time? Space?
;;

;;
;; Definition of procedure:
;;
(define (fact n)
  (if (= n 0)
      1 
      (* n (fact (- n 1)))))

;;
;; Let's 