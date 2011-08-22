;;
;; Exercise 04
;;
;; Now write a version of "expt" that takes less than O(n) time.
;;

;;
;; The old version of "expt":
;;
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;
;; New version of "expt" implemented using successive squaring:
;;
(define (expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (expt b (/ n 2))))
	(else
	 (* b (expt b (- n 1))))))

;;
;; Let's expand this procedure call using the substitution model:
;;
(expt 3 5)
(* 3 (expt 3 4))
(* 3 (square (expt 3 2)))
(* 3 (square (square (expt 3 1))))
(* 3 (square (square (* 3 (expt 3 0)))))
(* 3 (square (square (* 3 1))))
(* 3 (square (square 3)))
(* 3 (square 9))
(* 3 81)
243

;;
;; The procedure takes 10 operations in time, and consumes 5 slots in memory.
;;

;;
;; Now let's see how the procedure performs for n = 10:
;;
(expt 3 10)
(square (expt 3 5))

.. ==> 10 additional operations in time, and 5 additional slots in memory.

(square 243)
59049

;;
;; We made some pretty good savings in terms of time!
;;
;; If we double n, we only add 1 additional slot in memory, and 4 opertions in time.
;;

;;
;; Similarly, for n = 20:
;;
(expt 3 20)
(square (expt 3 10))

.. ==> 14 additional operations in time, and 6 additional slots in memory

(square 59049)
3486784401