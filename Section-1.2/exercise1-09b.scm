;;
;; Exercise 1.9(b)
;;
;; It is worth noting that the procedures defined in Exercise 1.9 (both recursive and iterative)
;; only work with positive a. Below we give definitions of procedures that work with any integer
;; value of a:
;;

(define (inc a) (- a -1)) ;; leave "+" alone, since we are re-defining it
(define (dec a) (- a 1))

;;
;; Recursive procedure defining recursive process:
;;
(define (+ a b)
  (cond ((= a 0) b)
	((> a 0) (inc (+ (dec a) b)))
	((< a 0) (dec (+ (inc a) b)))))

;;
;; Let's look at the call graph for (+ -4 5):
;;
(+ -4 5)
(dec (+ (inc -4) 5))
(dec (+ -3 5))
(dec (dec (+ (inc -3) 5)))
(dec (dec (+ -2 5)))
(dec (dec (dec (+ (inc -2) 5))))
(dec (dec (dec (+ -1 5))))
(dec (dec (dec (dec (+ (inc -1) 5)))))
(dec (dec (dec (dec (+ 0 5)))))
(dec (dec (dec (dec 5))))
(dec (dec (dec 4)))
(dec (dec 3))
(dec 2)
1

;;
;; Recursive procedure defining iterative process:
;;
(define (+ a b)
  (cond ((= a 0) b)
	((> a 0) (+ (dec a) (inc b)))
	((< a 0) (+ (inc a) (dec b)))))

;;
;; Again, let's look at the call graph for (+ -4 5):
;;
(+ -4 5)
(+ (inc -4) (dec 5))
(+ -3 4)
(+ (inc -3) (dec 4))
(+ -2 3)
(+ (inc -2) (dec 3))
(+ -1 2)
(+ (inc -1) (dec 2))
(+ 0 1)
1
