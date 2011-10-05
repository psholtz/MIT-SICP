;;
;; Exercise 1.26
;;
;; Louis Reasoner is having great difficulty doing exercise 1.24. This "fast-prime?" test seems to 
;; run more slowly than this "prime?" test. Louis calls his friend Eva Lu Ator over to help. When 
;; they examine Louis' code, they find that he has rewritten the expmod procedure to use an explicit
;; multiplication, rather than calling "square":
;;
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                      m))
;;          (else 
;;           (remainder (* base (expmod base (- exp 1) m))
;;                      m))))
;;
;; "I don't see what difference that could make," says Louis. "I do." says Eva. "By writing the 
;; "procedure like that you have transformed the O(log n) process into a O(n) process." Explain.
;;

;;
;; For the sake of entertainment, let's set ourselves up to run the prime number tests using the 
;; version of "expmod" given above:
;;
(defun even? (n) (= (% n 2) 0))
(defun square (n) (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (% (* (expmod base (/ exp 2) m)
	       (expmod base (/ exp 2) m))
	    m))
	(t
	 (% (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(t '())))

;;
;; Let's run some unit tests, to make sure it works:
;;
(setq n 100)

(fast-prime? 3 n)
;; ==> t
(fast-prime? 4 n)
;; ==> nil
(fast-prime? 5 n)
;; ==> t
(fast-prime? 6 n)
;; ==> nil
(fast-prime? 7 n)
;; ==> t

;;
;; Next, define the procedures for running timed tests.
;;
;; We'll change this somewhat from the procedures presented in the text
;; in that our procedure will only print a number (and corresponding time)
;; if it's prime.
;;
;; On MIT Scheme, the (runtime) procedure is given by (real-time-clock), and
;; this is the procedure we will use in our code.
;;