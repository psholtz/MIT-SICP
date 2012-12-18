;;
;; Exercise 2.77
;; 
;; Louis Reasoner tries to evaluate the expression (magnitude z) where z is the object 
;; shown in figure 2.24. To his surprise, instead of the answer 5 he gets an error message 
;; from apply-generic, saying there is no method for the operation magnitude on the types (complex). 
;; He shows this interaction to Alyssa P. Hacker, who says ``The problem is that the complex-number 
;; selectors were never defined for complex numbers, just for polar and rectangular numbers. 
;; All you have to do to make this work is add the following to the complex package:''
;;
;;  (put 'real-part '(complex) real-part)
;;  (put 'imag-part '(complex) imag-part)
;;  (put 'magnitude '(complex) magnitude)
;;  (put 'angle '(complex) angle)
;;
;; Describe in detail why this works. As an example, trace through all the procedures called in 
;; evaluating the expression (magnitude z) where z is the object shown in figure 2.24. In particular, 
;; how many times is apply-generic invoked? What procedure is dispatched to in each case?
;;

(load "numbers.scm")

;;
;; Although it is not stated explicitly in the problem statement, it is worth noting
;; that it is expected that the four selectors are already defined in the global 
;; environment/namespace, and that they are implemented as per the "data-directed"
;; generic model which was given in Section 2.4. 
;;
;; We give that definition here again, for reference.
;;

;;
;; First the generic application procedure:
;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

;;
;; And the selectors themselves:
;;
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;
;; Secondly, yes Alyssa is correct, the changes she proposes are all that is required
;; to make the package work correctly. Let's trace through the invocation given in the 
;; text to see why.
;;
(define z (make-complex-from-real-imag 3 4))
;; ==> (complex rectangular 3 . 4)

;;
;; Now let's examine the call graph:
;;
(magnitude z)
(apply-generic 'magnitude z)
;; type-tags <== (map type-tag (list z))
;; type-tags <== '(complex)
;; proc <== (get 'magnitude '(complex))
;; proc <== magnitude [DEFINED GLOBALLY]
(apply magnitude (map contents (list z)))
(apply magnitude (list '(rectangular 3 . 4))
(magnitude '(rectangular 3 . 4))
(apply-generic 'magnitude '(rectangular 3 . 4))
;; type-tags <== (map type-tag (list '(rectangular 3 . 4)))
;; type-tags <== '(rectangular)
;; proc <== (get 'magnitude 'rectangular)
;; proc <== magnitude [DEFINED IN RECTANGULAR PACKAGE]
(apply magnitude (map contents '(rectangular 3 . 4)))
(apply magnitude '((3 . 4)))
(magnitude (3 . 4))
(sqrt (+ (square (real-part (3 . 4)))
	 (square (imag-part (3 . 4)))))
(sqrt (+ (square 3)
	 (square 4)))
(sqrt (+ 9 16))
(sqrt 25)
5

;;
;; "apply-generic" is invoked twice.
;;
;; The first time, the procedure dispatches back to the "magnitude" procedure that is 
;; defined in the global namespace, and which was -- in fact -- the procedure that 
;; originally invoked "apply-generic" the first time.
;;
;; The second time, apply-generic dispatches to the "magnitude" procedure that is 
;; defined in the "rectangular" package.
;;