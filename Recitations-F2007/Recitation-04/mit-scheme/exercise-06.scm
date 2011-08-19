;;
;; Exercise 6
;;
;; Consider the recursive definition of factorial we've seen before:
;;
;; (define (fact n)
;;  (if (= n 0)
;;      1
;;      (* n (fact (- n 1)))))
;;
;; Running time? Space?
;;

;;
;; Definition of procedure:
;;
(define (fact n)
  (if (= n 0)
      1 
      (* n (fact (- n 1)))))

;;
;; Let's look at the expansion for n = 5, using the substitution model:
;;
(fact 5)
(* 5 (fact 4))
(* 5 (* 4 (fact 3)))
(* 5 (* 4 (* 3 (fact 2))))
(* 5 (* 4 (* 3 (* 2 (fact 1)))))
(* 5 (* 4 (* 3 (* 2 (* 1 (fact 0))))))
(* 5 (* 4 (* 3 (* 2 (* 1 1)))))
(* 5 (* 4 (* 3 (* 2 1))))
(* 5 (* 4 (* 3 2)))
(* 5 (* 4 6))
(* 5 24)
120

;;
;; The calculation consumes 11 steps (time), and requires 6 slots in memory (space).
;;

;;
;; Now let's look at the same expansion, using n = 7:
;;
(fact 7)
(* 7 (fact 6))
(* 7 (* 6 (fact 5)))

... ==> 11 additional steps in time, 6 additional slots in memory

(* 7 (* 6 120))
(* 7 720)
5040

;;
;; The calculation with n = 7 requires a total of 15 steps in time, and 8 slots in memory.
;;
;; From this we conclude that for an argument of size n, we require 2*n + 1 steps in time
;; and n + 1 slots in memory.
;;

;;
;; Both the time and the space required as linear in n.
;;

;;
;; Time: O(n)
;; Space: O(n)
;;