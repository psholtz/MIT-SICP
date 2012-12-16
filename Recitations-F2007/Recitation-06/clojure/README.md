Recitation 6
============ 

Contains worked solutions expressed in Clojure.

Clojure (obviously) allows both Lisp-style programming, but it also accomodates a more "Clojure-esque" style of programming, which we call the "Joy" style in reference to the "Joy of Clojure" programming book. We use two separate namespaces, one for each programming style, and give two separate sets of solutions for the questions.

Working Definitions
-------------------

**Lisp-style Definitions**
```clojure
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
```

**Clojure-style Definitions**
<pre>
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
</pre>
