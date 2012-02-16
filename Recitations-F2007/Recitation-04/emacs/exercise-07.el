;;                                                                                                                    
;; Exercise 7                                                                                                         
;;                                                                                                                    
;; Assume you have a procedure "(divisible? n x)" which returns #t if n is divisible                                  
;; by x. It runs in O(n) time and O(1) space. Write a procedure "prime?" which takes                                  
;; a number and returns #t if it's prime and #f otherwise. You'll want to use a                                       
;; helper procedure.                                                                                                  
;;        

;;                                                                                                                    
;; Define a "divisible?" procedure (so code will run):                                                                
;;   
(defun divisible? (a b)
  (= (rem* a b) 0))

(defun prime? (p)
  (defun helper (n)
    (if (> (sqrt p))
	t
      (if (divisible? p n)
	  '()
	(helper (+ n 1)))))
  (helper 2))

;;                                                                                                                    
;; There are really two "helper" methods for "helper" itself: "sqrt" and "divisible".                                 
;;                                                                                                                    
;; Supposing that "sqrt" runs in O(1) time, then we have divisible which requires O(n)                                
;; time, and we need to test sqrt(n) numbers using this divisible procedure. Hence, the                               
;; time it takes to run this procedure will be O(n * sqrt(n)), and the space it consumes                              
;; will be O(1).                                                                                                      
;;
