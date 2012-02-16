;;
;; Exercise 2
;;
;; What is the order of growth of the following procedure to calculate b^n by repeated multiplication?
;;
;; (define (expt b n)
;;  (if (= n 0)
;;      1
;;      (* b (expt b (- n 1)))))
;;
;; Running time? Space?
;;

;;
;; Define the procedure:
;;
(defun expt (b n)
  (if (= n 0)
      1
    (* b (expt b (- n 1)))))

;; 
;; Let's expand a few examples, using the substitution model:
;;
(expt 3 5)
(* 3 (expt 3 4))
(* 3 (* 3 (expt 3 3)))
(* 3 (* 3 (* 3 (expt 3 2))))
(* 3 (* 3 (* 3 (* 3 (expt 3 1)))))
(* 3 (* 3 (* 3 (* 3 (* 3 (expt 3 0))))))
(* 3 (* 3 (* 3 (* 3 (* 3 1)))))
(* 3 (* 3 (* 3 (* 3 3))))
(* 3 (* 3 (* 3 9)))
(* 3 (* 3 27))
(* 3 81)
243

;; 
;; It takes 11 steps (in time) to compute the answer, for n = 5
;; It takes 6 slots (in space) to compute the answer, for n = 5
;;

;; 
;; Let's compute another example, using n = 7 this time:
;;
(expt 3 7)
(* 3 (expt 3 6))
(* 3 (* 3 (expt 3 5)))

... ==> 11 additional steps in time, and 6 additional slots in space. 

(* 3 (* 3 243)
(* 3 729)
2187

;;
;; It takes 15 steps (in time) to compute the answer, for n = 7
;; It takes 8 slots (in space) to compute the answer, for n = 7
;;

;;
;; We can extrapolate from this that the running scales as 2*n + 1,
;; and that the space required scales as n + 1
;;
;; In other words, both running time and space scale linearly in n.
;;

;;
;; In both time and space, the order of growth is linear in n, O(n).
;;