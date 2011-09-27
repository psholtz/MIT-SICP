;;
;; Exercise 2.29
;;
;; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a
;; certain length, from which hangs either a weight or another binary mobile. We can represent a binary
;; mobile using compound data by constructing it from two branches (for example, using "list"):
;;
;; (define (make-mobile left right)
;;  (list left right))
;;
;; A branch is constructed from a "length" (which must be a number) together with a "structure" which 
;; may be either a number (representing a simple weight) or another mobile:
;;
;; (define (make-branch length structure)
;;  (list length structure))
;;

;;
;; (a) Write the corresponding selectors "left-branch" and "right-branch", which return the branches
;; of a mobile, and "branch-length" and "branch-structure" whihc return the components of a branch.
;;

;;
;; Define the "mobile" code:
;;
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;;
;; Define the "branch" code:
;;
(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;;
;; Let's define some sample mobiles:
;;
(define m1 (make-mobile 
	    (make-branch 1 1)
	    (make-branch 1 1)))

(define m2 (make-mobile
	    (make-branch 2 3)
	    (make-branch 3 2)))

(define m3 (make-mobile 
	    (make-branch 1 3)
	    (make-branch 3 5)))

(define m4 (make-mobile
	    (make-branch 3 m1)
	    (make-branch 3 m2)))

(define m5 (make-mobile 
	    (make-branch 3 5)
	    (make-branch 5 m4)))

;;
;; Unit test of "m1"
;;
(left-branch m1)
;; ==> (1 1)
(right-branch m1)
;; ==> (1 1)
(branch-length (left-branch m1))
;; ==> 1
(branch-length (right-branch m1))
;; ==> 1
(branch-structure (left-branch m1))
;; ==> 1
(branch-structure (right-branch m1))
;; ==> 1


;; 
;; Unit test of "m2"
;;
(left-branch m2)
;; ==> (2 3)
(right-branch m2)
;; ==> (3 2)
(branch-length (left-branch m2))
;; ==> 2
(branch-length (right-branch m2))
;; ==> 3
(branch-structure (left-branch m2))
;; ==> 3
(branch-structure (right-branch m2))
;; ==> 2


;;
;; Unit test of "m3"
;;
(left-branch m3)
;; ==> (1 3)
(right-branch m3)
;; ==> (3 5)
(branch-length (left-branch m3))
;; ==> 1
(branch-length (right-branch m3))
;; ==> 3
(branch-structure (left-branch m3))
;; ==> 3
(branch-structure (right-branch m3))
;; ==> 5


;;
;; Unit test of "m4"
;;
(left-branch m4)
;; ==> (3 ((1 1) (1 1)))
(right-branch m4)
;; ==> (3 ((2 3) (3 2)))
(branch-length (left-branch m4))
;; ==> 3
(branch-length (right-branch m4))
;; ==> 3
(branch-structure (left-branch m4))
;; ==> ((1 1) (1 1))
(branch-structure (right-branch m4))
;; ==> ((2 3) (3 2))
(left-branch (branch-structure (left-branch m4)))
;; ==> (1 1)
(right-branch (branch-structure (left-branch m4)))
;; ==> (1 1)
(left-branch (branch-structure (right-branch m4)))
;; ==> (2 3)
(right-branch (branch-structure (right-branch m4)))
;; ==> (3 2)


;;
;; Unit test of "m5"
;;
(left-branch m5)
;; ==> (3 5)
(right-branch m5)
;; ==> (5 ((3 ((1 1) (1 1))) (3 ((2 3) (3 2)))))
(branch-length (left-branch m5))
;; ==> 3
(branch-length (right-bracnh m5))
;; ==> 5
(branch-structure (left-branch m5))
;; ==> 5
(branch-structure (right-branch m5))
;; ==> ((3 ((1 1) (1 1))) (3 ((2 3) (3 2))))
(left-branch (branch-structure (right-branch m5)))
;; ==> (3 ((1 1) (1 1)))
(right-branch (branch-structure (right-branch m5)))
;; ==> (3 ((2 3) (3 2)))


;;
;; (b) Using your selectors, define a procedure "total-weight" that returns the total weight of a mobile.
;;

;;
;; "total-weight" may be defined recursively in terms of a second procedure "branch-weight" which returns
;; the weight of a particular branch:
;;
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (cond ((number? structure) structure)
	  (else
	   (total-weight structure)))))

(total-weight m1)
;; ==> 2
(total-weight m2)
;; ==> 5
(total-weight m3)
;; ==> 8
(total-weight m4)
;; ==> 7
(total-weight m5)
;; ==> 12


;;
;; (c) A mobile is said to be "balanced" if the torque applied by its top-left branch is equal to that 
;; applied by its top-right branch (that is, if the length of the left rod multiplied by the weight 
;; hanging from that rod is equal to the corresponding product for the right side), and if each of the 
;; submobiles hanging off its branches is balancecd. Design a procedure that tests whether a binary mobile
;; is balanced.
;;

;;
;; First let's design a "selector" for branches that returns the torque of the argument branch:
;;
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(torque (left-branch m1))
;; ==> 1
(torque (right-branch m1))
;; ==> 1
(torque (left-branch m2))
;; ==> 6
(torque (right-branch m2))
;; ==> 6
(torque (left-branch m3))
;; ==> 3
(torque (right-branch m3))
;; ==> 15
(torque (left-branch m4))
;; ==> 6
(torque (right-branch m4))
;; ==> 15
(torque (left-branch m5))
;; ==> 15
(torque (right-branch m5))
;; ==> 35


;;
;; Let's also define a selector "balanced-branch?" which determines whether the mobile attached
;; to the branch is balanced. If the attached structure is simply a weight, then we presume yes, 
;; the single weight is balanced. If the attach structure is a mobile, we return whether the 
;; mobile is balanced.
;;
(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (cond ((number? structure) #t)
	  (else
	   (balanced-mobile? structure)))))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (torque left) (torque right)) (balanced-branch? left) (balanced-branch? right))))

(balanced-mobile? m1)
;; ==> #t
(balanced-mobile? m2)
;; ==> #t
(balanced-mobile? m3)
;; ==> #f
(balanced-mobile? m4)
;; ==> #f
(balanced-mobile? m5)
;; ==> #f

;;
;; Let's construct a compound mobile, which is balanced:
;;
(define m6 (make-mobile
	    (make-branch 3 m1)
	    (nake-branch 3 m1)))

(balanced-mobile? m6)
;; ==> #t

;;
;; (d) Suppose we change the representation of mobiles so that the constructors are:
;; 
;; (define (make-mobile left right) (cons left right))
;; (define (make-branch length structure) (cons length structure))
;;
;; How much do you need to change your programs to convert to the new representations?
;;

;;
;; We only need to change the constructors, and the second of the two selectors attached to 
;; each objects, to wit, "right-branch" and "branch-structure" respectively. 
;;

;;
;; Using the new constructors, we have:
;;
(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch))
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;;
;; Let's reconstruct the test objects using our new constructors:
;;
(define m1 (make-mobile
	    (make-branch 1 1)
	    (make-branch 1 1)))

(define m2 (make-mobile
	    (make-branch 2 3)
	    (make-branch 3 2)))

(define m3 (make-mobile
	    (make-branch 1 3)
	    (make-branch 3 5)))

(define m4 (make-mobile
	    (make-branch 3 m1)
	    (make-branch 3 m2)))

(define m5 (make-mobile
	    (make-branch 3 5)
	    (make-branch 5 m4)))

(define m6 (make-mobile
	    (make-branch 3 m1)
	    (make-branch 3 m1)))

;;
;; Does the procedure "total-weight" still function as we anticipate?
;;
(total-weight m1)
;; ==> 2
(total-weight m2)
;; ==> 5
(total-weight m3)
;; ==> 8
(total-weight m4)
;; ==> 7
(total-weight m5)
;; ==> 12
(total-weight m6)
;; ==> 4

;;
;; Does the procedure "balanced-mobile?" still function as we anticipate?
;;
(balanced-mobile? m1)
;; ==> #t
(balanced-mobile? m2)
;; ==> #t
(balanced-mobile? m3)
;; ==> #f
(balanced-mobile? m4)
;; ==> #f
(balanced-mobile? m5)
;; ==> #f
(balanced-mobile? m6)
;; ==> #t