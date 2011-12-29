;;
;; Exercise 2.27
;;
;; Modify your "reverse" procedure of exercise 2.18 to produce a "deep-reverse" procedure
;; that takes a list as argument and returns as its value the list with its elements 
;; reversed and with all sublists deep-reversed as well. For example:
;;
;; (define x (list (list 1 2) (list 3 4)))
;; ==> ((1 2) (3 4))
;;
;; (reverse x)
;; ((3 4) (1 2))
;;
;; (deep-reverse x)
;; ((4 3) (2 1))
;;

;;
;; The "reverse" procedure of 2.18
;;
(defun reverse (original)
  (if (null original)
      '()
    (append (reverse (cdr original)) (list (car original)))))

;;
;; The simplest way to implement this procedure is to simply check whether each element
;; is itself a list, and if so, then recursively "reverse" that element (i.e., list) as well:
;;
(defun deep-reverse (original)
  (cond ((null original) '())
	((listp (car original))
	 (append (deep-reverse (cdr original)) (list (deep-reverse (car original)))))
	(t
	 (append (deep-reverse (cdr original)) (list (car original))))))

;; 
;; Run some use cases:
;;
(setq x (list (list 1 2) (list 3 4)))
;; ==> ((1 2) (3 4))
(reverse x)
;; ==> ((3 4) (1 2))
(deep-reverse x)
;; ==> ((4 3) (2 1))

(setq y (list (list (list 1 2) (list 3 4)) 5 6))
;; ==> (((1 2) (3 4)) 5 6)
(reverse y)
;; ==> (6 5 ((1 2) (3 4)))
(deep-reverse y)
;; ==> (6 5 ((4 3) (2 1)))