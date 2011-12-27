;;
;; We observed in the last exercise that the "par2" procedure seemed to produce tighter
;; error bounds than the "par1" procedure. Let's go back and look more closely at this:
;;

;;
;; Constructors and Selectors:
;;
(defun make-interval (a b) (cons a b))
(defun lower-bound (x) (car x))
(defun upper-bound (x) (cdr x))

;; 
;; Arithmetical Procedures:
;;
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x 
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;
;; Let's add the "make-center-width" and "make-center-percent" constructors:
;;
(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defun make-center-percent (c p)
  (let ((w (* c p)))
    (make-center-width c w)))
(defun percent (i)
  (/ (width i) (center i)))

;;
;; First look at the example with the extremely wide intervals:
;;
(setq x (make-center-percent 10 0.3))
(setq y (make-center-percent 10 0.4))
