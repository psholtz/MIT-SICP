(defn nth [seq n]
  (defn iter [seq k]
    (if (= k n)
      (first seq)
      (iter (rest seq) (+ k 1))))
  (iter seq 0))
