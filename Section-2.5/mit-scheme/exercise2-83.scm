;;
;; Exercise 2.83
;;
;; [WORKING]
;;

;;
;; Both the tower diagram and the problem statement refer to a type hierarchy 
;; something like the following:
;;
;;  integer --> rational --> real --> complex
;;
;; However, we have not hitherto generated "integer" and "real" packages, and, 
;; on the contrary, both "integer" and "real" types are handled uniformly by 
;; our "scheme-number" package. 
;;
;; Accordingly, we will define two hierarchies that looks something more like:
;;
;;  scheme-number ---> rational ---> complex
;;
;;
;; [WORKING --> rewrite this part]
;;
;; We will moreover define "helper" procedures that raise a scheme-number 
;; directly to a complex (if the scheme number is not a rational number), and 
;; which "lower" a rational number to a scheme-number (i.e., convert the 
;; rational representation to a real-number representation as a scheme-number).
;;

;;
;; This method uses the "exact-integer?" procedure which is built into MIT Scheme.
;;
(define (raise-scheme-number->rational n)
  ;; Helper method
  (define (raise-scheme-number->complex n)
    (make-complex-from-real-imag n 0.0))

  ;; Raise depending on the type
  (if (exact-integer? n)
      (make-rational n 1)
      (raise-scheme-number->complex n)))

;;
;; Raise a rational to complex by first converting it to a real 
;; (i.e., scheme-number):
;;
(define (raise-rational->complex r)
  ;; Helper method (determines if the argument is of type rational)
  (define (is-rational? x)
    (if (pair? x)
	(let ((kind (car x)))
	  (eq? kind 'rational))
	#f))

  ;; Helper method (lower the rational down to real/scheme-number)
  (define (lower-rational->scheme-number x)
    (let ((n (numer x))
	  (d (denom x)))
      (make-scheme-number (/ (* 1.0 n) (* 1.0 d)))))

  ;; Raise to complex
  (if (is-rational? r)
      (let ((x (lower-rational->scheme-number r)))
	(make-complex-from-real-imag x 0.0))
      ;; default, just return the value
      r))

;;
;; Run some unit tests:
;;
(raise-scheme-number->complex (make-scheme-number 3))
;; ==> (complex rectangular 3 . 0.)
(raise-scheme-number->complex (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0.)

(raise-scheme-number->rational (make-scheme-number 3))
;; ==> (rational 3 . 1)
(raise-scheme-number->rational (make-scheme-number 3.14))
;; ==> (complex rectangular 3.14 . 0.)

(lower-rational->scheme-number (make-rational 3 1))
;; ==> 3.
(lower-rational->scheme-number (make-rational 5 4))
;; ==> 1.25

(raise-rational->complex (make-rational 3 1))
;; ==> (complex rectangular 3. . 0.)
(raise-rational->complex (make-rational 5 4))
;; ==> (complex rectangular 1.25 . 0.)

;;
;; We define the generic "raise" procedure:
;;
(define (raise x) (apply-generic 'raise x))

;;
;; Update the operations table:
;;
(put 'raise '(scheme-number) raise-scheme-number->rational)
(put 'raise '(rational) raise-rational->complex)
    