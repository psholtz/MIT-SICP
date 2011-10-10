;;
;; Exercise 2.11
;; 
;; In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals, it is 
;; possible to break mul-interval into nine cases, only one of which requires more than two multiplications."
;; Rewrite this procedure using Ben's suggestion.
;;

;;
;; The nine cases can be categorized as follows:
;;
;; CASE I:    (and (> (lower-bound x) 0) (> (lower-bound y) 0))
;; CASE II:   (and (< (upper-bound x) 0) (< (upper-bound y) 0))
;; CASE III:  (and (> (lower-bound x) 0) (< (upper-bound y) 0))
;; CASE IV:   (and (< (upper-bound x) 0) (> (lower-bound y) 0))
;; CASE V:    (and (< (lower-bound x) 0) (> (upper-bound x) 0) (> (lower-bound y) 0))
;; CASE VI:   (and (< (lower-bound x) 0) (> (upper-bound x) 0) (< (upper-bound y) 0))
;; CASE VII:  (and (< (lower-bound y) 0) (> (upper-bound y) 0) (> (lower-bound x) 0))
;; CASE VIII: (and (< (lower-bound y) 0) (> (upper-bound y) 0) (< (upper-bound y) 0))
;; CASE IX:   all other cases, including boundaries being on zero. 
;; 

;;
;; The desired value, in each of the nine cases, is as follows:
;;
;; CASE I:    (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
;; CASE II:   (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
;; CASE III:  (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
;; CASE IV:   (make-interval (* (upper-bound y) (lower-bound x)) (* (lower-bound y) (upper-bound x)))
;; CASE V:    (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y)))
;; CASE VI:   (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y)))
;; CASE VII:  
;; CASE VIII:
;;
;; The last case is the one where we need to carry out all four multiplications, and can be implemented 
;; as the "old" mul-interval procedure was defined.
;;
(define (mul-interval x y)
  ;; CASE I
  (cond ((and (> (lower-bound x) 0) (> (lower-bound y) 0))
	 (make-interval 
	  (* (lower-bound x) (lower-bound y)) 
	  (* (upper-bound x) (upper-bound y))))

	;; CASE II
	((and (< (upper-bound x) 0) (< (upper-bound y) 0))
	 (make-interval 
	  (* (upper-bound x) (upper-bound y)) 
	  (* (lower-bound x) (lower-bound y))))

	;; CASE III
	((and (> (lower-bound x) 0) (< (upper-bound y) 0))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
       
	;; CASE IV
	((and (< (upper-bound x) 0) (> (lower-bound y) 0))
	 (make-interval (* (upper-bound y) (lower-bound x)) (* (lower-bound y) (upper-bound x))))

	;; CASE V
	((and (< (lower-bound x) 0) (> (upper-bound x) 0) (> (lower-bound y) 0))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))

	;; CASE VI
	((and (< (lower-bound x) 0) (> (upper-bound x) 0) (> (lower-bound y) 0))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))

	;; CASE VII
	((and (< (lower-bound y) 0) (> (upper-bound y) 0) (> (lower-bound x) 0))
	 (make-interval (* (upper-bound y) (lower-bound x)) (* (upper-bound y) (upper-bound x))))

	;; CASE VIII
	((and (< (lower-bound y) 0) (> (upper-bound y) 0) (< (upper-bound x) 0))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound y) (upper-bound x))))

	;; CASE IX
	(else
	 (let ((p1 (* (lower-bound x) (lower-bound y)))
	       (p2 (* (lower-bound x) (upper-bound y)))
	       (p3 (* (upper-bound x) (lower-bound y)))
	       (p4 (* (upper-bound x) (upper-bound y))))
	   (make-interval (min p1 p2 p3 p4)
			  (max p1 p2 p3 p4))))))

;;
;; For the sake of reference and comparison, let's define the old multiplication
;; scheme, so that we can compare if the answers agree between the two versions:
;;
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;
;; Define also the constructors and selectors, so we can work with intervals:
;;
(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; 
;; Case I Tests:
;;
(define p1 (make-interval 3 4))
(define p2 (make-interval 5 6))

(mul-interval p1 p2)
;; ==> (15 . 24)
(equal? (mul-interval p1 p2) (mul-interval-old p1 p2))
;; ==> #t

;; 
;; Case II Tests:
;;