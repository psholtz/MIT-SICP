;;
;; Working definitions
;;
(defn or? [exp]
  (and (list? exp) (= (first exp) 'or)))
(defn make-or [exp1 exp2]
  (list 'or exp1 exp2))
(defn or-first [exp]
  (first (rest exp)))
(defn or-second [exp]
  (first (rest (rest exp))))

(defn and? [exp]
  (and (list? exp) (= (first exp) 'and)))
(defn make-and [exp1 exp2]
  (list 'and exp1 exp2))
(defn and-first [exp]
  (first (rest exp)))
(defn and-second [exp]
  (first (rest (rest exp))))

;;
;; Exercise 2
;;
;; Write selectors, constructor and predicate for "not".
;;
(defn not? [exp]
  (and (list? exp) (= (first exp) 'not)))
(defn make-not [exp]
  (list 'not exp))
(defn not-first [exp]
  (first (rest exp)))