;;
;; Exercise 4
;;
;; Suppose x is bound to the list (1 2 3 4 5 6 7). Using "map", "filter" and/or "fold-right", write 
;; an expression involving "x" that returns:
;;
;; (a) (1 4 9 16 25 36 49)
;;
(setq x (list 1 2 3 4 5 6 7)

(defun map (proc lst)
  (if (null lst)
      '()
    (cons (funcall proc (car lst))
	  (map proc (cdr lst)))))

(defun square (x) (* x x))

(map #'square x)

;;
;; (b) (1 3 5 7)
;;
(defun filter (pred lst)
  (if (null lst)
      '()
    (if (funcall pred (car lst))
	(cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(defun odd? (x)
  (= (% x 2) 1))
(defun even? (x)
  (= (% x 2) 0)) 

(filter #'odd? x)

;;
;; (c) ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7))
;;

;;
;; One simple possibility is the following:
;;
(map #'(lambda (y) (list y y)) x)

;;
;; or the even more pathological:
;;
(map #'(lambda (x) (list x x)) x)

;;
;; (d) ((2) ((4) ((6) ())))
;;

;; 
;; First let's break down what this expression actually "is":
;;
(list (list 2) (list (list 4) (list (list 6) '())))
;; ==> ((2) ((4) ((6) nil)))

;;
;; Seeing what the pattern is, we can construct a function f that generates the structure we desire:
;;
(defun f (x)
  (let ((a (filter #'even? x)))
    (defun f-iter (b)
      (if (nullb)
          '()
	(list (list (car b)) (f-iter (cdr b)))))
    (f-iter a)))

;;
;; Running the procedure:
;;
(f x)
;; ==> ((2) ((4) ((6) nil)))

;;
;; Which is what we were looking for.
;;
;; However, this method uses a custom-defined procedure, rather than the predicates we were furnished with.
;; 
;; Let's see if we can rewrite it using the "fold-right" procedure:
;;
(defun fold-right (op init lst)
  (if (null lst)
      init
    (funcall op (car lst)
	     (fold-right op init (cdr lst)))))

(fold-right 
 (lambda (a b) (list (list a) b))
 '()
 (filter (lambda (y) (even? y)) x))
;; ==> ((2) ((4) ((6) nil)))

;;
;; Which again, is the answer we are seeking.
;;

;;
;; (e) The maximum element of x: 7
;;

;;
;; Let's walk down the list structure using "fold-right" and compare each two successive
;; elements, saving the largest one. We need to "initialize" the algorithm using a number
;; we know will be "smaller" than the smallest element in the list. In C/C++ we might set
;; this variable to something like (-1) * MAX_INT or some such value. 
;;
;; Here, let's just set this sentinel value to something like -1000, since we know that all 
;; the elements in the list will be larger than this value:
;;
(fold-right (lambda (a b) (if (> a b) a b))
	    -1000
	    x)
;; ==> 7

;;
;; (f) A list of the last element of x: (7)
;;

;;
;; One invocation which generates the desired response, using fold-right, is:
;;
(list (fold-right (lambda (a b) (if (null b) a b)) '() x))
;; ==> (7)

;;
;; (g) The list in reverse order: (7 6 5 4 3 2 1)
;;
(defun reverse (items)
  (fold-right (lambda (a b) (append b (list a))) '() items))

(reverse x)
;; ==> (7 6 5 4 3 2 1)

;;
;; (h) Bonus: reverse a list in less than O(n^2) time.
;;

;; 
;; The trick to getting a list reversal algorithm working in less than O(n^2) time
;; (or even, ideally, in linear time), is to do away with the call to "append", which 
;; is where the "performance hit" we are currently experiencing is taking place.
;;
;; In the implementation that follows, we replace the call with "append" with a call 
;; to "cons", which is much more efficient. The procedure uses an iterative "running 
;; total" of the reversed list that has been constructed thus far (i.e., "lst2").
;;
(defun reverse (items)
  (defun reverse-iter (lst1 lst2)
    (if(null lst1)
	lst2
      (reverse-iter (cdr lst1) (cons (car lst1) lst2))))
  (reverse-iteritems '()))

;;
;; Run some unit tests:
;;
(reverse x)
;; ==> '(7 6 5 4 3 2 1)

;;
;; Let's expand the call graph, to see just what is taking place here:
;;
(reverse '(1 2 3))
(reverse-iter '(1 2 3) '())
(reverse-iter '(2 3) '(1))
(reverse-iter '(3) '(2 1))
(reverse-iter '() '(3 2 1))
'(3 2 1)

;;
;; Clearly, the algorithm executes in O(n) time, and much more rapidly than the previous definition of reverse.
;;
