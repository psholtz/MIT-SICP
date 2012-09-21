Recitation 6
============ 

Contains worked solutions expressed in Clojure.

Working Definitions
-------------------

<pre>
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
</pre>