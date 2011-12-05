;;
;; Exercise 2.35
;;

;;
;; The original definition of "count-leaves":
;;
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else
	 (+ (count-leaves (car x))
	    (count-leaves (cdr x))))))

;;
;; Definition of accumulate:
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;
;; New definition of "count-leaves":
;;
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 
	      0
	      (map (lambda (x)
		     (if (pair? x)
			 (count-leaves x)
			 1)) t)))