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
;; In Emacs Lisp, we're going to use the built-in procedure "%" in place of "remainder". 
;;
;; In other words, we will be counting how many times evaluation of the expression (gcd 206 40) invokes "%".
;;

;;
;; First define the "gcd" procedure:
;;
(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (% a b))))

;;
;; Normal-order evaluation
;;
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 (% 206 40)))
(gcd 40 (% 206 40))
(if (= (% 206 40) 0) 40 (gcd (% 206 40) (% 40 (% 206 40))))

;; --> evaluation of (% 206 40) occurs here (+1)
(if (= 6 0) 40 (gcd (% 206 40) (% 40 (% 206 40))))
(gcd (% 206 40) (% 40 (% 206 40)))
(if (= (% 40 (% 206 40)) 0) (% 206 40) (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))

;; --> evaluation of (% 206 40) occurs here (+2)
(if (= (% 40 6) 0) (% 206 40) (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))

;; --> evaluation of (% 40 6) occurs here (+3)
(if (= 4 0) (% 206 40) (gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))
(gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))
(if (= (% (% 206 40) (% 40 (% 206 40))) 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))

;; --> evaluation of (% 206 40) occurs here (+4) 
(if (= (% 6 (% 40 (% 206 40))) 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))

;; --> evaluation of (% 206 40) occurs here (+5)
(if (= (% 6 (% 40 6)) 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))

;; --> evaluation of (% 40 6) occurs here (+6)
(if (= (% 6 4) 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))

;; --> evaluation of (% 6 4) occurs here (+7)
(if (= 2 0) (% 40 (% 206 40)) (gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))))
(gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))))