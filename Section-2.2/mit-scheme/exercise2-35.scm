;;
;; Exercise 2.35
;;
;; Redefine "count-leaves" from section 2.2.2 as an accumulation:
;;
;; (define (count-leaves t)
;;  (accumulate <??> <??> (map <??> <??>)))
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
;; One intuition would be to try to "flatten" the data structure, and then simply count
;; the length of the resulting list. For instance, given a structure such as (list (list 1 2) 3 4)
;; whose leaf number we known to be 4, we might like to transform it to a structure like (list 1 2 3 4)
;; and then simply count the length of the resulting list.
;;
;; The problem with this approach is that there is no clear way to implement such "flattening" 
;; by composing it with the "map" operator. Instead, what we will do is map a procedure onto the 
;; structure that return "1" if the element a leaf, and otherwise recurses down the structure until
;; it obtains a leaf. The result will be a structure that contains a "1" for every leaf in the original
;; structure. For there, we simply sum the elements in this list, to count the total number of leaves.
;;

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

(define x (list (list 1 2) 3 4))
(count-leaves x)
;; ==> 4

(define y (list (list 1 2) 3 4 (list 5 (list 6 (list 7 8)) 9 10)))
(count-leaves y)
;; ==> 10