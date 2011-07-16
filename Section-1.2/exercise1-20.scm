;;
;; Exercise 1.20
;;
;; The process that a procedure generates is of course dependent on the rules used by the interpreter. 
;; As an example, consider the iterative "gcd" procedure given above. Suppose we were to implement this 
;; procedure using normal-order evaluation, as discussed in section 1.1.5. Using the substitution method
;; (for normal order), illustrate the process generated in evaluating "(gcd 206 40)" and indicate the 
;; "remainder" operations that are actually performed. How many remainder operations are actually performed
;; in the normal-order evaluation of "(gcd 206 40)"? In the applicative-order evaluation?
;;

;; 
;; First define the "gcd" procedure:
;;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 
;; For simplicity, define "typedef" the remainder procedure:
;;
(define r remainder)

;; 
;; Normal-Order Evaluation
;;
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (r 206 40)))
(gcd 40 (r 206 40))
(if (= (r 206 40) 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))

;; --> evaluation of (r 206 40) occurs here (+1)
(if (= 6 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))     
(gcd (r 206 40) (r 40 (r 206 40)))
(if (= (r 40 (r 206 40)) 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

;; --> evaluation of (r 206 40) occurs here (+2) 
(if (= (r 40 6) 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

;; --> evaluation of (r 40 6) occurs here (+3)
(if (= 4 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
(if (= (r (r 206 40) (r 40 (r 206 40))) 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

;; --> evaluation of (r 206 40) occurs here (+4)
(if (= (r 6 (r 40 (r 206 40))) 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

;; --> evaluation of (r 206 40) occurs here (+5)
(if (= (r 6 (r 40 6)) 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

;; --> evaluation of (r 40 6) occurs here (+6)
(if (= (r 6 4) 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

;; --> evaluation of (r 6 4) occurs here (+7)
(if (= 2 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
(gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 206 40) occurs here (+8) 
(if (= (r (r 40 6) (r (r 206 40) (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 40 6) occurs here (+9)
(if (= (r 4 (r (r 206 40) (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
    
;; --> evaluation of (r 206 40) occurs here (+10)
(if (= (r 4 (r 6 (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 206 40) occurs here (+11)
(if (= (r 4 (r 6 (r 40 6))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 40 6) occurs here (+12)
(if (= (r 4 (r 6 4)) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 6 4) occurs here (+13)
(if (= (r 4 2) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; --> evaluation of (r 4 2) occurs here (+14)
(if (= 0 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
(r (r 206 40) (r 40 (r 206 40)))

;; --> evaluation of (r 206 40) occurs here (+15)
(r 6 (r 40 (r 206 40)))

;; --> evaluation of (r 206 40) occurs here (+16)
(r 6 (r 40 6))

;; --> evaluation of (r 40 6) occurs here (+17)
(r 6 4)

;; --> evaluation of (r 6 4) occurs here (+18)
2

;;
;; Using normal-order evaluation, the "remainder" procedure is invoked 18 times.
;;

;; ++++++++++++++++++++++++++++ 
;; Applicative-Order Evaluation
;; ++++++++++++++++++++++++++++ 
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (r 206 40)))
(gcd 40 (r 206 40))

;; --> evaluation of (r 206 40) occurs here (+1)
(gcd 40 6)          
(if (= 6 0) 40 (gcd 6 (r 40 6)))
(gcd 6 (r 40 6))

;; --> evaluation of (r 40 6) occurs here (+2)
(gcd 6 4)
(if (= 4 0) 6 (gcd 4 (r 6 4)))
(gcd 4 (r 6 4))

;; --> evaluation of (r 6 4) occurs here (+3)
(gcd 4 2)
(if (= 2 0) 4 (gcd 2 (r 4 2)))
(gcd 2 (r 4 2))

;; --> evaluation of (r 4 2) occurs here (+4)
(gcd 2 0)
(if (= 0 0) 2 (gcd 0 (r 2 0)))
2

;;
;; Using applicative-order evaluation, the "remainder" procedure is invoked 4 times.
;;