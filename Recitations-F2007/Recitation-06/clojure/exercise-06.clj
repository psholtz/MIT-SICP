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
    (filter predicate schedule)))x

;;
;; Basic Classes
;;
(def calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

;;
;; Exercise 6
;;
;; Finish the call to "make-student" to require the student takes at least 1 class.
;;
;; (make-student 575904476 ...)
;;

;;
;; First we need a constructor/factory for creating schedule checkers
;; that will validate that the student has elected at least one class.
;;
(defn make-schedule-checker-1 []
  (fn [schedule] (> (count schedule) 0)))


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
    (vec (filter predicate schedule))))

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

;;
;; Basic Classes
;;
(def calc1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calc2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

;;
;; Exercise 6
;;
;; Finish the call to "make-student" to require the student takes at least 1 class.
;;
;; (make-student 575904476 ...)
;;

;;
;; First we need a constructor/factory for creating schedule checkers
;; that will validate that the student has elected at least one class.
;;
(defn make-schedule-checker-1 []
  (fn [schedule] (> (count schedule) 0)))

;;
;; Let's run some unit tests:
;;
(def s1 (empty-schedule))
(def s2 (empty-schedule))
(def s2 (add-class calc1 s2))
(def s2 (add-class algebra s2))
(def s2 (add-class diff-eqs s2))

(def sid1 575904476)

(def student1 (make-student sid1 (make-schedule-checker-1)))

;;
;; Now try updating the student with schedules 1 and 2, respectively:
;;
(update-student-schedule student s1)
;; ==> "Invalid schedule!"

(update-student-schedule student s2)
;; ==> (575904476 [{:number CALC-101, :units {:C 4, :L 4, :H 4}} {:number ALGB-152, :units {:C 3, :L 3, :H 3}} {:number DIFF-201, :units {:C 3, :L 3, :H 3}}] #<joy$make_schedule_checker_1$fn__177 sicp.clojure.joy$make_schedule_checker_1$fn__177@5006279d>)