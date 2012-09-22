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
(defn cube [x] (* x x x))
(defn square [x] (* x x))

;;
;; Now define the fixed-point procedure:
;;
(defn fixed-point [f first-guess]
  (def tolerance 0.00001)
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess [guess number]
    (let [next (f guess)]
      (print "Guess number ")
      (print number)
      (print ": ")
      (println guess)
      (if (close-enough? guess next)
        next
        (try-guess next (+ number 1)))))
  (try-guess first-guess 1))

;;
;; Next define the procedures we need to support Newton's Method:
;;
(defn average [x y] (/ (+ x y) 2.0))
(defn average-damp [f] (fn [x] (average x (f x))))

;;
;; Define the derivative:
;;
(defn deriv [g]
  (def dx 0.00001)
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

;;
;; Define the Newton transform methods:
;;
(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

;;
;; Finally let's define the cubic procedure:
;;
(defn cubic [a b c] (fn [x] (+ (cube x) (* a (square x)) (* b x) c)))

;;
;; Let's run some simple tests.
;;
;; The root of x^3 is zero. Let's see if our procedure works for this use case:
;;
(newtons-method (cubic 0 0 ) 1.0)
;; ==> 2.6531990291797187E-5

;;
;; The root of x^3 + x^2 + x + 1 is -1:
;;
(newtons-method (cubic 1 1 1) 1.0)
;; ==>-0.9999999999997796

;;
;; Some other random use cases:
;;
(def x (newtons-method (cubic 1 2 10) 1.0))
;; ==> -2.182700432097661

((cubic 1 2 10) x)
;; ==> 2.759037442956469E-11