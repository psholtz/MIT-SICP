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

(adjoin-set 1 '())
;; ==> (1)
(adjoin-set 1 '(1))
;; ==> (1)
(adjoin-set 'z '(1 2 3))
;; ==> (z 1 2 3)

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

;;
;; An even more concise version of "union-set" could be given in terms of accumulate:
;;
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))

(define (union-set s1 s2)
  (accumulate (lambda (a b) (adjoin-set a b)) s2 s1))

;;
;; Let's run through the same unit tests again:
;;
(union-set '() '())
;; ==> ()
(union-set '(1 2 3) '())
;; ==> (1 2 3)
(union-set '() '(1 2 3))
;; ==> (1 2 3)
(union-set '(1 2 3) '(1 2 3))
;; ==> (1 2 3)
(union-set '(1) '(1 2 3))
;; ==> (1 2 3)
(union-set '(4) '(1 2 3))
;; ==> (4 1 2 3)
(union-set '(1 2 3) '(4 5 6))
;; ==> (1 2 3 4 5 6)

;; 
;; Let's compare the performance of these two versions of union.
;;

;;
;; The first use case, using the first algorithm:
;;
(union-set '(1 2 3) '(5 6 7 8 9))
(union-set-iter '(1 2 3) '(5 6 7 8 9))
(union-set-iter '(2 3) (adjoin-set 1 '(5 6 7 8 9)))
(union-set-iter '(3) (adjoin-set 2 (adjoin-set 1 '(5 6 7 8 9))))
(union-set-iter '() (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 '(5 6 7 8 9)))))
(adjoin-set 3 (adjoin-set 2 (adjoin-set 1 '(5 6 7 8 9))))
(adjoin-set 3 (adjoin-set 2 '(1 5 6 7 8 9)))   ;; 5 steps here to verify that 1 is not a member 
(adjoin-set 3 '(2 1 5 6 7 8 9))                ;; 6 steps here to verify that 2 is not a member
'(3 2 1 5 6 7 8 9)                             ;; 7 steps here to verify that 3 is not a member

;;
;; The total number of steps, then, is 9 + 5 + 6 + 7 = 27
;;

;;
;; The same use case, using the second algorithm:
;;
(define f (lambda (a b) (adjoin-set a b)))
(union-set '(1 2 3) '(5 6 7 8 9))
(accumulate f '(5 6 7 8 9) '(1 2 3))
(f 1 (accumulate f '(5 6 7 8 9) '(2 3)))
(f 1 (f 2 (accumulate f '(5 6 7 8 9) '(3))))
(f 1 (f 2 (f 3 (accumulate f '(5 6 7 8 9) '()))))
(f 1 (f 2 (f 3 '(5 6 7 8 9))))
(f 1 (f 2 '(3 5 6 7 8 9)))   ;; 5 steps here to verify that 3 is not a member
(f 1 '(2 3 5 6 7 8 9))       ;; 6 steps here to verify that 2 is not a member
'(1 2 3 4 5 6 7 8)           ;; 7 steps here to verify that 1 is not a member

;;
;; The total number of steps, then, is 10 + 5 + 6 + 7 = 28
;;

;;
;; A good deal of the processing time, then, seems to be spent in the "adjoin-set" 
;; procedure, checking the entire set to make sure that the element to be added
;; is not already part of the set.
;;

;;
;; The first algorithm is just a tiny bit more efficient, but the second algorithm
;; has the benefit of leaving the elements of the joined set in order. 
;;