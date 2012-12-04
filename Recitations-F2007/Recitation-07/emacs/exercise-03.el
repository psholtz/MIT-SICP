;;
;; Exercise 3
;;
;; Define "ls" to be a list of "procedures":
;;
;; (define (square x) (* x x))
;; (define (double x) (* x 2))
;; (define (inc x) (+ x 1))
;; (define ls (list square double inc))
;;
;; Now say we want a function "apply-procs" that behaves as follows:
;;
;; (apply-procs ls 4)
;;  ==> ((square 4) (double 4) (inc 4)) = (16 8 5)
;;
;; (apply-procs ls 3)
;; ==> ((square 3) (double 3) (inc 3)) = (9 6 4)
;;
;; Write a definition for "apply-procs" using "map".
;;

;;
;; First let's define the procedures that we need:
;;
(defun square (x) (* x x))
(defun double (x) (* x 2))
(defun inc (x) (+ x 1))
(defun ls (list #'square #'double #'inc))

;;
;; The following is a "naive" definition of "apply-procs" using simple recursion:
;;
(defun apply-process (a v)
  (if (null a)
      '()
    (cons (funcall (car a) v)
	  (apply-process (cdr a) v))))

;;
;; Run the unit tests:
;;
(apply-procs ls 4)
;; ==> (16 8 5)

(apply-procs ls 3)
;; ==> (9 6 4) 

;;
;; Now let's define "apply-procs" using the "map" procedure:
;;
(defun map (proc lst)
  (if (null lst)
      '()
    (cons (funcall proc (car lst))
	  (map proc (cdr lst)))))

(defun apply-procs (a v)
  (map #'(lambda (x) (funcall x v)) a))

;;
;; Running the unit test as above, we get the same results.
;; 