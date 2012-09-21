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
;; Definition of Simpson's Rule.
;;
;; In Clojure, the procedure "next" works much the same way that "cdr" does in more
;; conventional Lisp dialects. Accordingly, we will define our procedure using
;; "next-point" rather than simply "next".
;;
(defn simpson
  {:doc "Numericaly integrate f between a and b, dividing interval into n intervals."}
  [f a b n]

  ;; "Delta-x" by which to increase the integration point at each interval.
  (def h (/ (- b a) n))
  
  (defn next-point
    {:doc "Return the next integration point after x."}
    [x] (+ x h))
  
  (defn term
    {:doc "Return the value to use in the numerical integration at point x, at interval i."}
    [x i]
    (cond (= i 0) (f x)
          (= i n) (f x)
          (even? i) (* 2 (f x))
          (odd? i) (* 4 (f x))))

  ;; This definition is a slight modification/optimization over what is presented in the text.
  (defn sum
    {:doc "Recursive procedure to sum the numerical terms from x to b, where i is the step number in the integration."}
    [x i]
    (if (> x b)
      0
      (+ (term x i)
	 (sum (next-point x) (+ i 1)))))
                 
  ;;
  ;; Check to make sure we have even n.
  ;; Carry out the numerical integration using Simpson's rule.
  ;;
  (if (even? n)
    (* (/ h 3) (sum a 0))
    2))

;;
;; Now carry out the numerical integration on "cube" between 0 and 1 with n=100, n=1000
;; 
(defn cube
  {:doc "Return cube of argument."}
  [x] (* x x x))

(simpson cube 0 1 100)
;; --> 1/4
(simpson cube 0 1 1000)
;; --> 1/4

;;
;; This is the answer we were expecting.
;;

;;
;; We can perform the integration using real numbers:
;;
(simpson cube 0.0 1.0 100)
;; --> 0.246666666666

(simpson cube 0.0 1.0 1000)
;; --> 0.246666666666