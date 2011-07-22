;;
;; Exercise 1.15
;;

;; define the "cube" form
(defun cube (x) (* x x x))

;; define the "p" form
(defun p (x) (- (* 3 x) (* 4 (cube x))))

;; define the "sine" form
(defun sine (angle)
  (if (not (> (abs angle) 0.1))
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
-0.3980345741334

;;
;; p is invoked 5 times.
;;
;; The order of growth is O(ln(n)).
;;