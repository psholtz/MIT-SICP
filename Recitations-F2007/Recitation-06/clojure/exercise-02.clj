;;
;; Working definitions (Lisp-style)
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

;;
;; Exercise 2
;;
;; Write a selector that when given a class and a schedule, returns a
;; new schedule including the new class:
;;
(defn
  ^{:doc "'schedule' must already be a list"}
  add-class [class schedule]
  (concat schedule (list class)))

;;
;; Run some unit tests.
;;
;; First define some units and classes:
;;
(def u1 (make-units 3 3 3))
(def calc1 (make-class 101 u1))
(def calc2 (make-class 102 u1))

;;
;; Now try to build a schedule using these:
;;
(def s (add-class calc1 (empty-schedule)))
;; ==> #'user/s
(def s (add-class calc2 s))
;; ==> #'user/s

;;
;; Inspect the schedule:
;;
(first s)
;; ==> (101 (3 3 3))

(first (rest s))
;; ==> (102 (3 3 3))

;;
;; The order of growth in both time and space is linear in the variable
;; "schedule", that is, it is O(n) where "n" is the length of the list
;; structure "schedule".
;;

;; =========================================================================

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

;;
;; Solution
;;
(defn
  ^{:doc "'schedule' must already be a vector"}
  add-class [class schedule]
  (conj schedule class))