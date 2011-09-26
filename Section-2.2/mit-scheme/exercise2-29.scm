;;
;; Exercise 2.29
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

;;
;; Let's also define a selector "balanced-branch?" which determines whether the mobile attached
;; to the branch is balanced. If the attached structure is simply a weight, then we presume yes, 
;; the single weight is balanced. If the attach structure is a mobile, we return whether the 
;; mobile is balanced.
;;
(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (cond ((number? structure) #f)
	  (else
	   (balanced-mobile? structure)))))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (torque left) (torque right)) (balanced-branch? left) (balanced-branch? right))))

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
;; each objects, to wit, "right-brach" and "branch-structure" respectively. 
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

(define (length-branch branch)
  (car branch))

(define (structure-branch branch)
  (cdr branch))