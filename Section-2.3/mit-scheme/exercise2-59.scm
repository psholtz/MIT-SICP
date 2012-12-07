;;
;; Exercise 2.59
;;
;; Implement the "union-set" operation for the unordered-list representation of sets.
;;

;;
;; First define the supporting procedures:
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(1 2 3))
;; ==> #t
(element-of-set? 'a '(1 2 3))
;; ==> #f

;;
;; Define the "adjoin-set" procedure:
;;
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;
;; Now define the union (most of the logic is already in "adjoin-set"):
;;
(define (union-set s1 s2)
  (define (union-set-iter t1 t2)
    (if (null? t1)
	t2
	(union-set-iter (cdr t1) (adjoin-set (car t1) t2))))
  (union-set-iter s1 s2))

(union-set '() '())
;; ==> ()
(union-set '(1 2 3) '())
;; ==> (3 2 1)
(union-set '() '(1 2 3))
;; ==> (1 2 3)
(union-set '(1 2 3) '(1 2 3))
;; ==> (1 2 3)
(union-set '(1) '(1 2 3))
;; ==> (1 2 3)
(union-set '(4) '(1 2 3))
;; ==> (4 1 2 3)
(union-set '(1 2 3) '(4 5 6))
;; ==> (3 2 1 4 5 6)

