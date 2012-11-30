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
  (defn predicate [class]
    (not (same-class? (get-class-number class) classnum)))
  (vec (filter predicate schedule)))

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
;; Exercise 7
;;
;; Finish the call to "make-student" to create a first-term freshman (limited to 54 units).
;;

;;
;; Define the appropriate constructor:
;;
(defn make-schedule-checker-2 [max-units]
  (fn [schedule] (<= (total-scheduled-units schedule) max-units)))

;;
;; Let's run some unit tests:
;;
(def s1 (empty-schedule))
(def s2 (empty-schedule))
(def s2 (add-class calc1 s2))
(def s2 (add-class algebra s2))
(def s2 (add-class diff-eqs s2))
(def s3 (add-class us-history s3))
(def s3 (add-class world-history s3))
(def s4 s3)
(def s4 (add-class basket-weaving s4))

(total-scheduled-units s1)
;; ==> 0
(total-scheduled-units s2)
;; ==> 30
(total-scheduled-units s3)
;; ==> 54
(total-scheduled-units s4)
;; ==> 57

(def sid1 575904476)

(def student1 (make-student sid1 (make-schedule-checker-2 54)))

;;
;; Now try updating the student with the respective schedules:
;;
(get-student-schedule (update-student-schedule student1 s1))
;; ==> []
(get-student-schedule (update-student-schedule student1 s2))
;; ==> [{:number CALC-101, :units {:C 4, :L 4, :H 4}} {:number ALGB-152, :units {:C 3, :L 3, :H 3}} {:number DIFF-201, :units {:C 3, :L 3, :H 3}}]
(get-student-schedule (update-student-schedule student1 s3))
;; ==> [{:number CALC-101, :units {:C 4, :L 4, :H 4}} {:number ALGB-152, :units {:C 3, :L 3, :H 3}} {:number DIFF-201, :units {:C 3, :L 3, :H 3}} {:number HIST-122, :units {:C 4, :L 4, :H 4}} {:number HIST-324, :units {:C 4, :L 4, :H 4}}]
(get-student-schedule (update-student-schedule student1 s4))
;; ==> "Invalid schedule!"