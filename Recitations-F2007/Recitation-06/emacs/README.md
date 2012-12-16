Recitation 6
============ 

Contains worked solutions expressed in the Emacs dialect of Common Lisp.

Core code that is used throughout these examples:

**Data Structures**

```lisp
(defun make-units (C L H)
 (list C L H))
(defun get-units-C (x) (car x))
(defun get-units-L (x) (cadr x))
(defun get-units-H (x) (cadr (cdr x)))

(defun make-class (number units)
 (list number units))
(defun get-class-number (x) (car x))
(defun get-class-units (x) (cadr x))

(defun get-class-total-units (class)
 (let ((units (get-class-units class)))
  (+ 
   (get-units-C units)
   (get-units-L units)
   (get-units-H units))))

(defun same-class? (c1 c2)
 (equal (get-class-number c1) (get-class-number c2)))
```

**HOPs**

```lisp
(defun make-student (number schedule-checker)
 (list number (list) schedule-checker))
(defun get-student-number (x) (car x))
(defun get-student-schedule (x) (cadr x))
(defun get-student-checker (x) (cadr (cdr x)))

;;
;; Emacs does not handle HOPs gracefully, hence the "let":
;;
(defun update-student-schedule (student schedule)
 (let ((test-function (get-student-checker student)))
  (if (funcall test-function schedule)
      (list (get-student-number student)
            schedule
	    test-function)
       (error "Invalid schedule!"))))
```
