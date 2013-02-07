;;
;; Exercise 1.8
;;
;; Although not requested, I thought it would be useful to 
;; bundle all the cube-root code together into a single block, 
;; as demonstrated in the text.
;;

;; define the "cube" form
(defun cube (x) (* x x x))

;;
;; Block defining the cube root procedure.
;;
(defun cube-root (x)
  (defvar tolerance 0.001)
  (defun good-enough? (guess)
    (< (abs (- (/ (cube guess) x) 1.0)) tolerance))
  (defun improve (guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (defun cube-root-iter (guess)
    (if (good-enough? guess)
	guess
      (cube-root-iter (improve guess))))
  (cube-root-iter 1.0))

;;
;; Run some unit tests:
;;
(cube-root 2)
;; ==> 1.259933493449977

(cube-root 3)
;; ==> 1.4422497895989996

(cube-root 8)
;; ==> 2.000004911675504