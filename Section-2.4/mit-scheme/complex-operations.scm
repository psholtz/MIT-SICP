;;
;; General operations that can be used and defined
;; on complex numbers, independent of the data 
;; representation used to define them:
;;
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;;
;; Let's define the "table" that we will use for "put/get":
;;
;; (This is from peeking ahead into Section 3 of the text)
;;
