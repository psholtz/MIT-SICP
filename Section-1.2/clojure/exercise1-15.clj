;;
;; Exercise 1.15
;;
;; The sine of an angle (specified in radians) can be computed by making use of the approximation
;; sin x = x for small x, and the trigonometric identity:
;;
;;  sin x = 3 * sin(x/3) - 4 * sin^3(x/3)
;;
;; to reduce the size of the argument to sin.
;;
;; We will set the "sufficiently small" tolerance in this example to be 0.1
;;
;; Use these ideas to implement a sine procedure.
;;

;; define the "cube" form
(defn cube
  {:doc "Return the cube of the argument"}
  [n] (* n n n))

;; define the "p" form
(defn p
  {:doc "Procedure that defines the approximation to sine"}
  [x] (- (* 3 x) (* 4 (cube x))))

;; define the "sine" form
(defn sine
  {:doc "Define an approximation to sine for small angles"}
  [angle]
  (if (not (> (Math/abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;;
;; We follow the call graph when (sine 12.15) is evaluated:
;;
(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine (/ 4.05 3.0))))
(p (p (sine 1.35)))
(p (p (p (sine (/ 1.35 3.0)))))
(p (p (p (sine 0.45))))
(p (p (p (p (sine (/ 0.45 3.0))))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine (/ 0.15 3.0)))))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
(p (p (p (p 0.1495))))
(p (p (p 0.4351345505)))
(p (p 0.9758465332))
(p -0.78956311447)
-0.39980345741334

;;
;; p is invoked 5 times.
;;
;; With increasing n, the function (sine n) is invoked recursively with n/3.
;;
;; The order of growth is logarithmic with n, that is, O(ln(n)).
;;