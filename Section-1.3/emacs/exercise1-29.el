;;
;; Exercise 1.29
;;
;; Simpson's Rule is a more accurate method of numerical integration than the method
;; illustrated above. Using Simpson's Rule, the integral of a function f between 
;; a and b is approximated by:
;;
;; (h/3)*(y0 + 4*y1 + 2*y2 + 4*y3 + 2*y4 + ... + 2*y(n-2) + 4*y(n-1) + y(n))
;;
;; where h=(b-a)/n, for some even integer n, and y(k) = f(a+kh). (Increasing n increases
;; the accuracy of the approximation). Define a procedure that takes as arguments f, a, b
;; and n and returns the value of the integral, computed using Simpson's Rule. Use your 
;; procedure to integrate "cube" between 0 and 1 (with n = 100 and n = 1000), and compare 
;; the results to those of the "integral" procedure shown above.
;;

;;
;; Increase the buffers
;;
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;;
;; Definition of Simpson's Rule:
;;
(defun simpson (f a b n)
  (defun even? (k) (= (% k 2) 0))
  (defun odd? (k) (not (even? k)))
  (setq h (/ (- b a) n))
  (defun next (x) (+ x h))
  (defun term (a i n)
    (cond ((= i 0) (funcall f a))
	  ((= i n) (funcall f a))
	  ((even? i) (* 2 (funcall f a)))
	  ((odd? i) (* 4 (funcall f a)))))
  (defun sum (term a next b i n)
    (if (> a b)
	0
      (+ (funcall term a i n)
	 (sum term (funcall next a) next b (+ i 1) n))))

  ;;
  ;; Check to make sure we have even n.
  ;;
  (if (even? n)
      (* (/ h 3) (sum #'term a #'next b 0 n))
    '()))

;;
;; Now carry out the numerical integration on "cube" between 0 and 1 with n=100, n=1000
;; 
(defun cube (n) (* n n n))

;;
;; Any of the following four expressions exceeds the variable binding depth available to emacs:
(simpson #'cube 0 1 100)
(simpson #'cube 0 1 1000)
(simpson #'cube 0.0 1.0 100.0)
(simpson #'cube 0.0 1.0 1000.0)

;;
;; In fact, even the following expression exceeds the variable binding depth available to emacs:
;;
(simpson #'cube 0 1 2)