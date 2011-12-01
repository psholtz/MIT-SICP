;;
;; Exercise 2.32
;;
;; We can represent a set as a list of distinct elements, and we can represent the set of all
;; subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of 
;; all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition
;; of a procedure that generates the set of all subsets of a set and give a clear explanation
;; of why it works:
;;
;; (define (subsets s)
;;  (if (null? s)
;;      (list '())
;;      (let ((rest (subsets (cdr s))))
;;       (append rest (map <??> rest)))))
;;
;;
;; The "power set" of a set (or, the set of all sets) can be obtained by the following algorithm:
;; 
;; (1) Remove an element E from the set S, and label the set S/E = S'
;; (2) Calculate the power set of S' 
;; (3) Combine (a) the power set of S' and (b) the set obtained by joining the element E to each
;;     element of the power set of S'. 
;; (4) If S' was non-empty, return to step (1), replacing S with S'.
;; (5) Continue until the generated set  S' is empty.
;;

;;
;; In the sample definition given, if "s" is empty, we return the empty set: (list '())
;; If not, we pluck out an element E from the set (i.e., (car s)), we calculate the power
;; set of the remaining elements of s, and we must then join E back into the elements of 
;; this generated power set. It is this final step that the "mystery lambda" must perform. 
;; In the expression:
;;
;; (append rest (map ?? rest))
;; 
;; the "rest" indicates the power set of the remaining elements. To generate the "entire"
;; power set, we must join "rest" with the set obtained by joining the removed element E
;; back into the power set. We do this by iterating over the elements of rest, and "joining"
;; (using the "car" operator) the element E back into the set.
;;
;; The definition of the "mystery lambda" that we seek is:
;;
;; (lambda (x) (cons (car s) x))
;;
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;
;; Run some use cases:
;;
(subsets (list))
;; ==> (())
(subsets (list 1))
;; ==> (() (1))
(subsets (list 1 2))
;; ==> (() (2) (1) (1 2))
(subsets (list 1 2 3))
;; ==> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(subsets (list 'a 'b))
;; ==> (() (b) (a) (a b))