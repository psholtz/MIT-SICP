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
