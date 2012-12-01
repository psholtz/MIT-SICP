;;
;; Working definition (Lisp-style)
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
;; Working definitions (HOPs)
;;
(defn make-student [number sched-checker]
  (list number (list) sched-checker))
(defn get-student-number [x] (first x))
(defn get-student-schedule [x] (first (rest x)))
(defn get-student-checker [x] (first (rest (rest x))))

(defn update-student-schedule [student schedule]
  (if ((get-student-checker student) schedule)
    (list (get-student-number student)
	  schedule
	  (get-student-checker student))
    (throw (Exception. "Invalid schedule!"))))

;;
;; Previous solutions
;;
(defn empty-schedule [] '())

(defn add-class [class schedule]
  (concat schedule (list class)))

(defn total-scheduled-units [schedule]
  (defn total-scheduled-units-iter [total working]
    (if (empty? working)
      total
      (let [current-class (first working)]
	(total-scheduled-units-iter
	 (+ total
	    (get-class-total-units current-class))
	 (rest working)))))
  (total-scheduled-units-iter 0 schedule))

(defn drop-class [schedule classnum]
  (let [temp-class (make-class classnum '())]
    (defn predicate [class]
      (not (same-class? class temp-class)))
    (filter predicate schedule)))

(defn credit-limit [schedule max-credits]
  (defn credit-limit-iter [working]
    (if (empty? working)
      '()
      (let [total-credits (total-scheduled-units working)
	    first-class (first working)]
	(if (> total-credits max-credits)
	  (credit-limit-iter
	   (drop-class working (get-class-number first-class)))
	  working))))
  (credit-limit-iter schedule))

(defn make-schedule-checker-1 []
  (fn [schedule] (> (count schedule) 0)))

(defn make-schedule-checker-2 [max-units]
  (fn [schedule] (<= (total-scheduled-units schedule) max-units)))

;;
;; Basic Classes
;;
(def calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))
(def us-history (make-class 'HIST-122 (make-units 4 4 4)))
(def world-history (make-class 'HIST-324 (make-units 4 4 4)))
(def basket-weaving (make-class 'BASKETS (make-units 1 1 1)))

;;
;; Exercise 8
;;
;; Write a procedure that takes a schedule and returns a list of the class numbers in the schedule. Use map.
;;
(defn class-numbers [schedule]
  (map (fn [x] (get-class-number x)) schedule))

;;
;; Let's run some unit tests:
;;
(define s1 (empty-schedule))
(define s2 (empty-schedule))
(define s2 (add-class calc1 s2))
(define s2 (add-class algebra s2))
(define s2 (add-class diff-eqs s2))
(define s3 s2)
(define s3 (add-class us-history s3))
(define s3 (add-class world-history s3))
(define s4 s3)
(define s4 (add-class basket-weaving s4))

(class-numbers s1)
;; ==> ()
(class-numbers s2)
;; ==> (CALC-101 ALGB-152 DIFF-201)
(class-numbers s3)
;; ==> (CALC-101 ALGB-152 DIFF-201 HIST-122 HIST-324)
(class-numbers s4)
;; ==> (CALC-101 ALGB-152 DIFF-201 HIST-122 HIST-324 BASKETS)

;; ==============================================================

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
;; Working definitions (HOPs)
;;
(defn make-student [number sched-checker]
  {:number number :schedule [] :checker sched-checker})
(defn get-student-number [x] (x :number))
(defn get-student-schedule [x] (x :schedule))
(defn get-student-checker [x] (x :checker))

(defn update-student-schedule [student schedule]
  (if ((get-student-checker student) schedule)
    {:number (get-student-number student)
     :schedule schedule
     :checker (get-student-checker student)}
    (throw (Exception. "Invalid schedule!"))))

;;
;; Previous solutions
;;
(defn empty-schedule [] [])

(defn add-class [class schedule]
  (conj schedule class))

(defn total-scheduled-units [schedule]
  (defn total-scheduled-units-iter [total working]
    (if (empty? working)
      total
      (let [current-class (first working)]
	(total-scheduled-units-iter
	 (+ total
	    (get-class-total-units current-class))
	 (subvec working 1)))))
  (total-scheduled-units-iter 0 schedule))

(defn drop-class [schedule classnum]
  (let [temp-class (make-class classnum '())]
    (defn predicate [class]
      (not (same-class? class temp-class)))
    (filter predicate schedule)))

(defn credit-limit [schedule max-credits]
  (defn credit-limit-iter [working]
    (if (empty? working)
      []
      (let [total-credits (total-scheduled-units working)
	    first-class (first working)]
	(if (> total-credits max-credits)
	  (credit-limit-iter (drop-class working (get-class-number first-class)))
	  working))))
  (credit-limit-iter schedule))

(defn make-schedule-checker-1 []
  (fn [schedule] (> (count schedule) 0)))

(defn make-schedule-checker-2 [max-units]
  (fn [schedule] (<= (total-scheduled-units schedule) max-units)))

;;
;; Basic Classes
;;
(def calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))
(def us-history (make-class 'HIST-122 (make-units 4 4 4)))
(def world-history (make-class 'HIST-324 (make-units 4 4 4)))
(def basket-weaving (make-class 'BASKETS (make-units 1 1 1)))

;;
;; Exercise 8
;;
;; Write a procedure that takes a schedule and returns a list of the class numbers in the schedule. Use map.
;;
(defn class-numbers [schedule]
  ;;
  ;; Case these to a "vector":
  ;;
  (vec (map (fn [x] (get-class-number x)) schedule)))

;;
;; Let's run some unit tests:
;;
(define s1 (empty-schedule))
(define s2 (empty-schedule))
(define s2 (add-class calc1 s2))
(define s2 (add-class algebra s2))
(define s2 (add-class diff-eqs s2))
(define s3 s2)
(define s3 (add-class us-history s3))
(define s3 (add-class world-history s3))
(define s4 s3)
(define s4 (add-class basket-weaving s4))

(class-numbers s1)
;; ==> []
(class-numbers s2)
;; ==> [CALC-101 ALGB-152 DIFF-201]
(class-numbers s3)
;; ==> [CALC-101 ALGB-152 DIFF-201 HIST-122 HIST-324]
(class-numbers s4)
;; ==> [CALC-101 ALGB-152 DIFF-201 HIST-122 HIST-324 BASKETS]

