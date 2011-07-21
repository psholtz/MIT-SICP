;;
;; Exercise 1.8
;;
;; Implement a procedure to calculate cube roots.
;;

;; define the "cube" form 
(defun cube (x) (* x x x))

;;
;; Interface to "cube-root-iter" procedure.
;;
(defun cube-root (x)
  (cube-root-iter 1.0 x))

;;
;; Iterative procedure that narrows down the cube root.
;;
(defun cube-root-iter (guess x)
  (if (good-enough? guess x)
      guess
    (cube-root-iter (improve guess x) x)))

;;
;; Use the "new" method of approximation based on fractional differences.
;;
(defun good-enough? (guess x)
  (setq tolerance 0.001)
  (< (abs (- (/ (cube guess) x) 1.0)) tolerance))

;;
;; Newton's method of approximation for cube roots.
;;
(defun improve (guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

