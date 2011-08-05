;;
;; 


;;
;;answer
;;
(define (sub-interval x y)
  (define (negative interval)
    (make-interval (* -1 (upper-bound interval))
		   (* -1 (lower-bound interval))))
  (add-interval x (negative y)))


(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))



(define (make-interval lower upper)
  (cons lower upper))
(define (negative interval)
  (make-interval (* -1 (upper interval)) 
		 (* -1 (lower interval))))

(D
(define (sub-interval x y)
  (let ((negative 

(define (sub-interval x y)
