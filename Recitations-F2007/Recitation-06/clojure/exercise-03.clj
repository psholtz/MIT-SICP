;;
;; Working definitions (Lisp style)
;;
(ns sicp.clojure.lisp)

(defn make-units [C L H] (list C L H))
(defn get-units-C [x] (first x))
(defn get-units-L [x] (first (rest x)))
(defn get-units-H [x] (first (rest (rest x))))

(defn make-class [number units] (list number units))
(defn get-class-number [x] (first x))
(defn get-class-units [x] (first (rest x)))

(defn get-class-total-units [class]
  (let [units (get-class-units class)]
    (+ (get-units-C units)
       (get-units-L units)
       (get-units-H units))))

(defn same-class? [c1 c2]
  (= (get-class-number c1) (get-class-number c2)))

;;
;; Previous solutions
;;
(defn empty-schedule [] '())

(defn
  ^{:doc "'schedule' must already be a list"}
  add-class [class schedule]
  (concat schedule (list class)))

;;
;; Exercise 3
;;
;; Write a selector that takes in a schedule and returns the total number
;; of units in that schedule:
;;
(defn
  ^{:doc "Return the total number of units in an entire schedule"}
  total-scehduled-units [schedule]
  
  (defn
    ^{:doc "Iterative process to recursively count the number of units."}
    total-scheduled-units-iter [total working]
    (if (empty? working)
      total
      (let [current-class (first working)]
        (total-schedued-units-iter
         (+ total
            (get-class-total-units current-class))
         (rest working)))))

  ;; Invoke the iterative process
  (total-scheduled-units-iter 0 schedule))

;;
;; Run some unit tests:
;;
(def calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calculus-2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(get-class-total-units calculus-1)
;; ==> 12
(get-class-total-units calculus-2)
;; ==> 12
(get-class-total-units algebra)
;; ==> 9 
(get-class-total-units diff-eqs)
;; ==> 9

(def s1 (empty-schedule))
(total-scheduled-units s1)
;; ==> 0

(def s1 (add-class calculus-1 s1))
;; ==> #'sicp.clojure.lisp/s1
(total-scheduled-units s1)
;; ==> 12

(def s1 (add-class algebra s1))
;; ==> #'sicp.clojure.lisp/s1
(total-scheduled-units s1)
;; ==> 21

(def s1 (add-class diff-eqs s1))
;; ==> #'sicp.clojure.lisp/s1
(total-scheduled-units s1)
;; ==> 30

;;
;; The order of the growth is time in linear in the size of the
;; variable "schedule", since we have to iterate over the entire
;; length of the variable to count all the units. Calculating the
;; number of units at each node of "schedule" is a constant-time
;; operation, and hence, the order of growth in time is O(n), where
;; n is the size of the variable "schedule".
;;
;; The order of growth in space is linear in the size of the
;; variable "schedule", presuming that we do not have to make
;; new copies of the variable at each invocation of the iteration
;; procedure; that is, presuming that the (cdr seq) call does not
;; create a copy of the sequence. If no copies are made, then only
;; the first copy of the variable "schedule" is used, and so the
;; order of growth in space is O(n), or linear in the size of the
;; variable "schedule".
;;
;; If copies of the variable are made at each procedure invocation,
;; then the order of growth statistics are O(n^2), where n is the
;; size of the variable "schedule". Specifically, n(n+1)/2, or roughly
;; (1/2)n^2 units of memory must be allocated to accomodate execution
;; of the procedure, since first then size-n "schedule" variable is
;; used, then the size-(n-1) "cdr" of that variable is used, and so on,
;; down to size 1.
;;

;; =============================================================================

;;
;; Working definitions (Joy-style)
;;
(ns sicp.clojure.joy)

(defn make-units [C L H] {:C C :L L :H H})
(defn get-units-C [x] (x :C))
(defn get-units-L [x] (x :L))
(defn get-units-H [x] (x :H))

(defn make-class [number units] {:number number :units units})
(defn get-class-number [x] (x :number))
(defn get-class-units [x] (x :units))

(defn get-class-total-units [class]
  (let [units (get-class-units class)]
    (+ (get-units-C units)
       (get-units-L units)
       (get-units-H units))))

(defn same-class? [c1 c2]
  (= (get-class-number c1) (get-class-number c2)))

;;
;; Previous solutions
;;
(defn empty-schedule [] [])

(defn
  ^{:doc "'schedule' must already be a vector"}
  add-class [class schedule]
  (conj schedule class))

;;
;; Solution
;;
(defn
  ^{:doc "Return the total number of units in an entire schedule."}
  total-scheduled-units [schedule]

  (defn
    ^{:doc "Iterative process to recursively count the number of units."}
    total-scheduled-units-iter [total working]
    (if (empty? working)
      total
      (let [current-class (first working)]
        (total-scheduled-units-iter
         (+ total
            (get-class-total-units current-class))
         (subvec working 1)))))

  ;; Invoke the iterative process
  (total-scheduled-units-iter 0 schedule))

;;
;; Run some unit tests:
;;
(def calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calculus-2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(get-class-total-units calculus-1)
;; ==> 12
(get-class-total-units calculus-2)
;; ==> 12
(get-class-total-units algebra)
;; ==> 9
(get-class-total-units diff-eqs)
;; ==> 9

(def s1 (empty-schedule))
(total-scheduled-units s1)
;; ==> 0

(def s1 (add-class calculus-1 s1))
;; ==> #'sicp.clojure.joy/s1
(total-scheduled-units s1)
;; ==> 12

(def s1 (add-class algebra s1))
;; ==> #'sicp.clojure.joy/s1
(total-scheduled-units s1)
;; ==> 21

(def s1 (add-class diff-eqs s1))
;; ==> #'sicp.clojure.joy/s1
(total-scheduled-units s1)
;; ==> 30