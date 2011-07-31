;;
;; Define a procedure "cubic" that can be used together with the "newtons-method" procedure in expressions of the form
;;
;; (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;;

;;
;; Let's define all the functions we need to make this example work.
;;
;; First let's define some supporting polynomial procedures:
;;
(define (cube n) (* n n n))
(define (square n) (n n))

;;
;; Now define the fixed-point procedure:
;;
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess number)
    (let ((next (f guess)))
      (newline)
      (display "Guess number ")
      (display number)
      (display ": ")
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try next (+ number 1)))))
  (try first-guess 1))

;;
;; Next define the procedures we need to support Newton's Method:
;;
(define (average x y) (/ (+ x y) 2.0))
(define (average-damp f) (lambda (x) (average x (f x))))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;
;; Finally let's define the cubic procedure:
;;
(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;;
;; Let's run some simple tests.
;; 
;; The root of x^3 is zero. Let's see if our procedure works for this use case:
;;
(newtons-method (cubic 0 0 0) 1.0)
;; ==> 2.653e-5

;;
;; The root of x^3 + x^2 + x + 1 is -1:
;;
(newtons-method (cubic 1 1 1) 1.0)
;; ==> -0.9999999999 

;;
;; Some other random use cases:
;;
(define x (newtons-method (cubic 1 2 10) 1.0))
;; ==> -2.182709907

((cubic 1 2 10) x)
;; ==> 2.759037e-11