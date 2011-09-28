;;
;; Exercise 1.19
;;
;; There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps.
;; Recall the transformation of the state variables a and b in the "fib-iter" process of section
;; 1.2.2:  a <-- a + b, and b <-- a. Call this transformation T, and observe that applying T over
;; and over again n times, starting with 1 and 0, produces the pair Fib(n+1) and Fib(n). In other
;; words, the Fibonacci numbers are produced by applying T^n, the nth power of the transformation T,
;; starting with the pair (1,0). Now consider T to be the special case of p=0 and q=1 in the family
;; of transformaitonss T(pq), where T(pq) transforms the pair (a,b) according to a <-- bq + aq + ap
;; and b <-- bp + aq. Show that if we apply such a transformation T(pq) twice, the effect is the same
;; as using a single transformation T(p'q') of the same form, and compute p' and q' in terms of p
;; and q. This gives us an explicit way to square these transformations, and thus we can compute T^n
;; using successive squaring, as in the fast-expt procedure. Put this all together to complete the
;; following procedure, which runs in a logarithmic number of steps.
;;
;; (define (fib n)
;;  (fib-iter 1 0 0 1 n))
;; (define (fib-iter a b p q count)
;;  (cond ((= count 0) b)
;;        ((even? count)
;;         (fib-iter a
;;                   b
;;                   <??>   ; compute p'
;;                   <??>   ; compute q'
;;                   (/ count 2)))
;;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;;                         (+ (* b p) (* a q))
;;                         p
;;                         q
;;                         (- count 1)))))
;;

(defn square [n] (* n n))

(defn sum-of-squares [a b]
  (+ (square a) (square b)))

(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count) (fib-iter a
                                b
                                (sum-of-squares p q)
                                (+ (square q) (* 2 p q))
                                (/ count 2))
        :else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))
        
(defn fib [n]
  (fib-iter 1 0 0 1 n))

;;
;; Run some simple unit tests
;;
(= (fib 0) 0)
(= (fib 1) 1)
(= (fib 2) 1)
(= (fib 3) 2)
(= (fib 4) 3)
(= (fib 5) 5)
(= (fib 6) 8)
(= (fib 7) 13)
(= (fib 8) 21)
(= (fib 9) 34)
(= (fib 10) 55)

;;
;; This code will hang an interpreter using a naive recursive process:
;;
(= (fib 102) (+ (fib 100) (fib 101)))

;;
;; Clojure is a much more robust Lisp implementation than most.
;;
;; On MIT Scheme, we can only get up to about (fib 100) or so before the interpreter begins max-ing out.
;;
;; With Clojure, we can easily compute numbers the size of (fib 1000) and (much) higher.
;;
(fib 1000)
;; ==> 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875