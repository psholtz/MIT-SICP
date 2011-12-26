
(defun mul-interval (x y)
  ;;
  ;; CASE I:
  ;;
  ;;   0  x x
  ;; --+-------
  ;;   0  y y 
  ;;
  (cond ((and (> (lower-bound x) 0) (> (lower-bound y) 0))
	 (make-interval 
	  (* (lower-bound x) (lower-bound y))
	  (* (upper-bound x) (upper-bound y))))

	;;
	;; CASE II:
	;; 
	;;   x x  0
	;; -------+--
	;;   y y  0 
	;;
	((and (< (upper-bound x) 0) (< (upper-bound y) 0))
	 (make-interval 
	  (* (upper-bound x) (upper-bound y))
	  (* (lower-bound x) (lower-bound y))))

	;;
	;; CASE III:
	;;    
	;;        0  x x
	;; -------+------- 
	;;   y y  0
	;;
	((and (> (lower-bound x) 0) (< (upper-bound y) 0))
	 (make-interval
	  (* (upper-bound x) (lower-bound y))
	  (* (lower-bound x) (upper-bound y))))

	;;
	;; CASE IV:
	;;
	;;   x x  0 
	;; -------+-------
	;;        0  y y
	;;
	((and (< (upper-bound x) 0) (> (lower-bound y) 0))
	 (make-interval (* (upper-bound y) (lower-bound x)) (* (lower-bound y) (upper-bound x))))

	;;
	;; CASE IX:
	;;
	;; Everything else!
	;;
	(t
	 (let ((p1 (* (lower-bound x) (lower-bound y)))
	       (p2 (* (lower-bound x) (upper-bound y)))
	       (p3 (* (upper-bound x) (lower-bound y)))
	       (p4 (* (upper-bound x) (upper-bound y))))
	   (make-interval (min p1 p2 p3 p4)
			  (max p1 p2 p3 p4))))))

;;
;; For the sake of reference and comparison, let's define the old multiplication
;; scheme, so that we can compare if the answers agree between the two versions:
;;
(defun mul-interval-old (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;
;; Define also the constructors and selectors, so we can work with intervals.
;;
;; Add (simple) type-checking to constructor, to make sure we are working with
;; a valid range:
;;
(defun make-interval (a b)
  (cond ((< a b) (cons a b))
	(t 
	 (princ "error constructing interval!"))))
(defun lower-bound (x) (car x))
(defun upper-bound (x) (cdr x))