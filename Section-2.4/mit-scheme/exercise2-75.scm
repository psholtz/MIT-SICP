;;
;; Exercise 2.75
;;
;; [WORKING]
;;

;;
;; The definition of "make-from-real-imag" given in the text is as follows:
;;
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude)
	   (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle) (atan y x))
	  (else
	   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; 
;; WORKING --> run unit tests for this version
;;


;;
;; Now, define the constructor "make-from-mag-ang" as requested:
;;
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part)
	   (* r (cos a)))
	  ((eq? op 'imag-part)
	   (* r (sin a)))
	  (else 
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;
;; WORKING --> run through the unit tests
;;