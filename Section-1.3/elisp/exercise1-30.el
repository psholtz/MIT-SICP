;;
;; Exercise 1.30
;;

;;
;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;;       (sum term (next a) next b))))
;;

;;
;; The sum procedure above generates a linear recursion. The procedure can be rewritten so that
;; the sum is performed iteratively. Show how to do this by filling in the missing expresions:
;;

;;
;; ================================================================================================ 
;; In the first place, the procedure above is defined in Scheme. 
;;
;; To make the problem workable in elisp, we must first re-write the original code in elisp. 
;; 
;; We will use the examples furnished in the text, whereby "term" is given by the "sum" procedure, 
;; and "next" is given by the "inc" procedure.
;; ================================================================================================ 
;;
(defun inc (n) (+ n 1))
(defun cube (x) (* x x x))

(defun sum (term a next b)
  (if (> a b)
      0
    (+ (funcall term a)
       (sum term (funcall next a) next b))))

(sum #'cube 1 #'inc 10)
;; --> 3025

;; 
;; Now let's define an iterative version of the "sum" procedure:
;;
(defun sum (term a next b)
  (defun iter (a result)
    (if (> a b)
	result
      (iter (funcall next a) (+ result (funcall term a)))))
  (iter a 0))

(sum #'cube 1 #'inc 10)
;; --> 3025