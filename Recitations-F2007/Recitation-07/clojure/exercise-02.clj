;;
;; Define "length" using a higher order list predicate.
;;

;;
;; Presently "length" is defined as follows:
;;
(defn length [lst]
  (if (empty? lst)
    0
    (+ 1 (length (rest lst)))))

;;
;; We can define "length" in terms of "fold-right" (or "accumulate") as follows:
;;
(defn fold-right [op init lst]
  (if (empty? lst)
    init
    (op (first lst)
	(fold-right op init (rest lst)))))

(defn length [lst]
  (fold-right (fn [a b] (+ b 1)) 0 lst))

;;
;; Run some unit tests:
;;
(define x '(1 1 2 1 3))
(length x)
;; ==> 5

(define y '(1 2 3 4 5 6 7))
(length y)
;; ==> 7