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
(def s1 (add-class calculus-2 s1))
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

;;
;; In a worst-case scenario, we need to step through all "n" elements of
;; the schedule structure, and at each step, we need to  invoke the
;; "total-scheduleded-units" procedure, which itself runs on O(n) time.
;; Walking down the list structure costs "n" steps, and invoking
;; "total-scheduled-units" at each nodes costs a total of an additional
;; (1/2)*(n)*(n+1) steps, so the total number of steps involved thus
;; far is (1/2)*(n^2+3*n).
;;
;; Furthermore, in a worst-case scenario we need to invoke the "drop-class"
;; procedure at each node, which is also linear in "n". Invoking this linear-time
;; procedure at each step of the structure will add an additional n*(n+1)/2
;; steps to the computation. The total number of steps required (in a worst
;; case scenario) will be n^2 + 2*n, so the procedure (in a worst-case
;; scenario) will run in O(n^2) time.
;;
;; In most instances, the procedure will run much more quickly than this.
;;
;; To calculate the space requirements, let's assume that the "total-scheduled-units"
;; procedure requires O(n) linear space. In a worst case scenario, we need to create
;; a new copy of the schedule, of size (n-1), at each step of the procedure
;; using the "drop-class" procedure. "drop-class" is linear in space, but
;; creating a new copy of the structure, of size (n-1), at each step, will require
;; a total of n(n+1)/2 units of memory. Hence the space requirements for the
;; algorithm - in a worst-case scenario - are O(n^2).
;;

;; =================================================================================

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
    (not (= (get-class-number class) classnum)))
  (vec (filter predicate schedule)))

;;
;; Exercise 5
;;
;; Enforce a credit limit by taking in a schedule, and removing classes until
;; the total number of units is less than max-credits.
;;
(defn credit-limit [schedule max-credits]
  '())

