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
;; We will moreover define "helper" procedures that raise a scheme-number 
;; directly to a complex (if the scheme number is not a rational number), and 
;; which "lower" a rational number to a scheme-number (i.e., convert the 
;; rational representation to a real-number representation as a scheme-number).
;;

;;
;; This method uses the "exact-integer?" procedure which is built into MIT Scheme.
;;
;; If the scheme-number is an "exact-integer", it is raised to a rational number,
;; otherwise, the real scheme-number is used to construct (i.e., raise) a complex 
;; number.
;;
(define (raise-scheme-number->rational n)
  (if (exact-integer? n)
      (make-rational n 1)
      (raise-scheme-number->complex n)))

(define (raise-scheme-number->complex n)
  (make-complex-from-real-imag n 0.0))

;;
;; This procedure should not be inserted into the operations table 
;; (at least, not without introducing cycles into the coercion graph, 
;; which we don't want to contend with right now). It is, however, a 
;; useful helper method in raising rational numbers to complex.
;;
(define (lower-rational->scheme-number r)
  (let ((n (numer r))
	(d (denom r)))
    (make-scheme-number (/ (* 1.0 n) (* 1.0 d)))))

;;
;; Raise a rational to complex by first converting it to a real 
;; (i.e., scheme-number):
;;
(define (raise-rational->complex r)
  (let ((x (lower-rational->scheme-number r)))
    (make-complex-from-real-imag x 0.0)))

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
;; Of the four methods defined above, although two need to be inserted into 
;; the operations table: raise-scheme-number->rational and raise-rational->complex.
;; The other two, raise-scheme-number->complex and lower-rational->scheme-numnber 
;; are helper methods which can be implemented as inner procedures.
;;

;;
;; We define the generic "raise" procedure:
;;
(define (raise x) (apply-generic 'raise x))



(put 'raise '(scheme-number)
     (lambda (x) (attach-tag 'ration

(put 'raise '(integer)
     (lambda (x) (attach-tag 'rational (raise-integer x))))
(put 'raise '(rational)
     (lambda (x) (attach-tag 'real (raise-rational x))))
(put 'raise '(real)
     (lambda (x) (attach-tag 'complex (raise-real x))))