;;
;; Exercise 1.30
;;

;;
;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;; 	 (sum term (next a) next b))))
;;

;;
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that 
;; the sum is performed iteratively. Show how to do this by filling in the missing expressions:
;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
