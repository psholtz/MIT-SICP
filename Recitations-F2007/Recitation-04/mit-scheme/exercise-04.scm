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