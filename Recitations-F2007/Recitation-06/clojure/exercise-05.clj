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
  (defn predicate [class]
    (not (= (get-class-number class) classnum)))
  (filter predicate schedule))

;;
;; Exercise 5
;;
;; Enforce a credit limit by taking in a schedule, and removing classes until
;; the total number of units is less than max-credits.
;;
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

;;
;; Run some unit tests:
;;
(def calculus-1 (make-class 'CALC-101 (make-units 4 4 4)))
(def calculus-2 (make-class 'CALC-102 (make-units 4 4 4)))
(def algebra (make-class 'ALGB-152 (make-units 3 3 3)))
(def diff-eqs (make-class 'DIFF-201 (make-units 3 3 3)))

(def s1 (empty-schedule))
(def s1 (add-class calculus-1 s1))
(def s1 (add-class calculus-1 s1))
(def s1 (add-class algebra s1))
(def s1 (add-class diff-eqs s1))

;;
;; Introspect s1:
;;
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))
;;

;;
;; Total number of units in s1:
;;
(total-scheduled-units s1)
;; ==> 42

;;
;; First test the empty case:
;;
(credit-limit '() 10)
;; ==> '()

;;
;; Then test the "do nothing" case:
;;
(credit-limit s1 50)
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 42)
;; ==> ((CALC-101 (4 4 4)) (CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 41)
;; ==> ((CALC-102 (4 4 4)) (ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(credit-limit s1 25)
;; ==> ((ALGB-152 (3 3 3)) (DIFF-201 (3 3 3)))

(total-scheduled-units (credit-limit s1 42))
;; ==> 42
(total-scheduled-units (credit-limit s1 41))
;; ==> 30
(total-scheduled-units (credit-limit s1 30))
;; ==> 30
(total-scheduled-units (credit-limit s1 25))
;; ==> 18

;; [WORKING --> order of growth]

;; =================================================================================

;; [WORKGIN]