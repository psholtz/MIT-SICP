;;
;; We can define these procedures for the fun of doing so in Emacs, 
;; although they are not that useful for our purposes. Emacs cannot
;; compute primes large enough to generate interesting statistics.
;;
(defun even? (n) (= (% n 2) 0))
(defun square (n) (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (% (square (expmod base (/ exp 2) m)) m))
	(t 
	 (% (* base (expmod base (- exp 1) m)) m))))

;;
;; Define the prime-testing procedure
;;
(defun prime? (n)
  (defun get-random-a ()
    (+ 2 (random (- n 4))))
  (defun test (a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 2) t)
	((= n 3) t)
	((= n 4) '())
	((= n 5) t)
	(t
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))

;; avg : list -> float
;; average value for a list of numbers
(defun avg (a)
  (defun avg-iter (b v i)
    (cond ((> (length b) 0)
	   (avg-iter (cdr b) (+ (car b) v) (+ i 1)))
	  (t 
	   (/ v (length a)))))
  (avg-iter a 0.0 0.0))

;; std : list -> float
;; standard deviation for a list of numbers
(defun std (d)
  (let ((a (avg d)))
    (defun std-iter (b v)
      (if (> (length b) 0)
	  (std-iter (cdr b) (+ (square (- (car b) a)) v))
	(sqrt (/ v (- (length d) 1)))))
    (if (= (length d) 1)
	0.0
      (std-iter d 0.0))))

;;
;; No point in defining the further distributions for this problem.
;;
	