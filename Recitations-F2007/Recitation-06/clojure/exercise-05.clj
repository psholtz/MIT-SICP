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
      working
      (let [total (total-scheduled-units working)
            current-class (first working)]
        (if (> total max-credits)
          (credit-limit-iter
           (drop-class (get-class-number current-class) schedule))
          working))))
  (credit-limit-iter schedule))
