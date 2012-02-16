;;
;; Exercise 3
;; 
;; Rewrite the previous definition so that it yields an iterative process.
;;

;;
;; The original definition:
;;
(defun expt (b n)
  (if (= n 0)
      1
    (* b (expt b (- n 1)))))

;;
;; The iterative definition:
;;
(defun expt (b n)
  (defun expt-iter (c total)
    (if (= c n)
	total
      (expt-iter (+ c 1) (* total b))))
  (expt-iter 0 1))

;;
;; Let's again use the substitution model to evaluate performance for n = 5:
;;
(expt 3 5)
(expt-iter 0 1)
(expt-iter 1 3)
(expt-iter 2 9)
(expt-iter 3 27)
(expt-iter 4 81)
(expt-iter 5 243)
243

;;
;; In time, the procedure requires 7 steps to complete, and it consumes a constant amount of space.
;;

;; 
;; Let's do this again for n = 7:
;;
(expt 3 7)
(expt-iter 0 1)
(expt-iter 1 3)
(expt-iter 2 9)
(expt-iter 3 27)
(expt-iter 4 81)
(expt-iter 5 243)
(expt-iter 6 729)
(expt-iter 7 2187)
2187

;;
;; In time, the procedure requires 9 steps to complete, and it consumes a constant amount of space.
;;

;;
;; We can extrapolate, that it time the procedure will require n+2 steps to complete, and consume a 
;; constant amount of space.
;;

;;
;; Hence, the procedure exhibits linear growth in time, and constant growth in space.
;;

;;
;; Time: O(n)
;; Space: O(1)
;;