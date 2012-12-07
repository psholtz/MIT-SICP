;;
;; [working]
;;

;; --> tghese are bags, or multisets

;;
;; "element-of-set?" can remain defined as before, since as soon as we find the first element in 
;; set, we are done:
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else
	 (element-of-set? x (cdr set)))))

;;
;; "adjoin-set" will be changed, since we no longer have to check for membership before 
;; adjoining/adding a member:
;;
(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (define (union-set-iter t1 t2)
    (if (null? t1)
	t2
	(union-set-iter (cdr t1) (adjoin-set (car t1) t2))))
  (union-set-iter s1 s2))  

;;
;; Sets are used in relational database technology. 
;;
;; For instance, we can return a set of tuples. 

;;
;; For instance, the field of data mining, Jaccard similarity is an index
;; that is used for bag.. The Jaccard similarity of two bag/multisets 
;; is defined as:
;; 