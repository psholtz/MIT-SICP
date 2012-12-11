;;
;; Alyssa's representation of complex numbers.
;; She has chosen to use the polar representation.
;;
(define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
(define (make-from-mag-ang r a) (cons r a)
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))

;;
;; Tagging the data
;;
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;;
;; Alyssa now needs to update her methods:
;;
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
(define (magnitude-polar  z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

;;
;; The four selectors above can now be written as:
;;
(define (real-part z)
  (cond ((rectangular? z)
	  (real-part-rectangular (contents z)))
	((polar? z)
	  (real-part-polar (contents z)))
	(else 
	  (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
	  (imag-part-rectangular (contents z)))
	((polar? z)
	  (imag-part-polar (contents z)))
	(else 
	  (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
	  (magnitude-rectangular (contents z)))
	((polar? z)
	  (magnitude-polar (contents z)))
	(else
	  (error "Unknown type -- MAGNTIUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
	  (angle-rectangular (contents z)))
	((polar? z)
	  (angle-polar (contents z)))
	(else 
	  (error "Unknown type -- ANGLE" z))))

;;
;; We can likewise redefine the following "general" constructors:
;;
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))