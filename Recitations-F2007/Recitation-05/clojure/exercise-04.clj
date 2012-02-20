;;
;; Exericse 4
;;
;; Now extend the point abstraction to handle line segments, with a constructor
;; "(make-line-segment p1 p2)" and selectors "line-segment-start" and "line-segment-end".
;;

;;
;; Define the constructor for the line segments:
;;
(defn make-line-segment [p1 p2]
  (list p1 p2))

;;
;; Define the selectors:
;;
(defn line-segment-start [line-segment]
  (first line-segment))

(defn line-segment-end [line-segment]
  (first (rest line-segment)))