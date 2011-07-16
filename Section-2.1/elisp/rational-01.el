;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Code from test implementing rational number arithmetic.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; Constructor for building rational numbers.
;;
(defun make-rat (n d) (cons n d))

;;
;; Extract the numerator.
;;
(defun numer (x) (car x))

;; 
;; Extract the denominator.
;;
(defun denom (x) (cdr x))

;;
;; Add two rational numbers together.
;;
(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

;;
;; Subtract two rational numbers.
;;
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

;; 
;; Multiply two rational numbers.
;;
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

;;
;; Divide two rational numbers.
;;
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

;;
;; Equality comparator.
;;
(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; 
;; Implement pretty-printing for rational numbers.
;;
(defun print-rat (x)
  (progn
    (prin1 (numer x))
    (princ "/")
    (prin1 (denom x))
    (princ "\n")
    nil))