;;
;; Exercise 2.1
;;
;; Define a better version of "make-rat" that handles both positive and negative argument.
;; "make-rat" should normalize the sign so that if the rational number is positive, both
;; the numerator and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.
;;

;;
;; Modified version of the "make-rat" constructor, incorporating the "gcd" test as in text:
;;
(defun make-rat (n d)
  (defun gcd (a b)
    (if (= b 0)
	a
      (gcd b (% a b))))
  (defun negative? (x)
    (< x 0))
  (let ((g (gcd (abs n) (abs d))))
    (if ( negative? d)
	(cons (/ (* -1 n) g) (/ (* -1 d) g))
      (cons (/ n g) (/ d g)))))

;;
;; Let's define the "print-rat" procedure so we can run some unit tests.
;;
(defun print-rat (x)
  (newline)
  (princ (numer x))
  (princ "/")
  (princ (denom x))
  (newline))

;;
;; Need selectors for numerator and denominator as well:
;;
(defun numer (x) (car x))
(defun denom (x) (cdr x))

;;
;; Run the unit tests:
;;
(print-rat (make-rat 1 2))
;; ==> 1/2
(print-rat (make-rat -1 2))
;; ==> -1/2
(print-rat (make-rat 1 -2))
;; ==> -1/2
(print-rat (make-rat -1 -2))
;; ==> 1/2
(print-rat (make-rat 0 2))
;; ==> 0/1

;;
;; Note that we're not testing for 0 in the denominator yet.
;;
(print-rat (make-rat 2 4))
;; ==> 1/2
(print-rat (make-rat -2 4))
;; ==> -1/2
(print-rat (make-rat -2 -4))
;; ==> 1/2
(print-rat (make-rat 2 -4))
;; ==> -1/2
(print-rat (make-rat 3 9))
;; ==> 1/3
(print-rat (make-rat 15 -9))
;; ==> -5/3