
;;
;; [WORKING]
;;

;;
;; Defin the "mobile" code:
;;
(defun make-mobile (left right)
  (list left right))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

;;
;; Define the "branch" code:
;;
(defun make-branch (length structure)
  (list length structure))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

;;
;; Let's define some sample mobiles:
;;
(setq m1 (make-mobile 
	  (make-branch 1 1)
	  (make-branch 1 1)))

(setq m2 (make-mobile 
	  (make-branch 2 3)
	  (make-branch 3 2)))

(setq m3 (make-mobile
	  (make-branch 1 3)
	  (make-branch 3 5)))

(setq m4 (make-mobile
	  (make-branch 3 m1)
	  (make-branch 3 m2)))

(setq m5 (make-mobile
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
;; ==> 2
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
(branch-length (right-branch m5))
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
(defun total-weight (mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(defun branch-weight (branch)
  (let ((structure (branch-structure branch)))
    (cond ((numberp structure) structure)
	  (t
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
(defun torque (branch)
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
(defun balanced-branch? (branch)
  (let ((structure (branch-structure branch)))
    (cond ((numberp structure) t)
	  (t
	   (balanced-mobile? structure)))))

(defun balanced-mobile? (mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (= (torque left) (torque right))
	 (balanced-branch? left) (balanced-branch? right))))

(balanced-mobile? m1)
;; ==> t
(balanced-mobile? m2)
;; ==> t
(balanced-mobile? m3)
;; ==> nil
(balanced-mobile? m4)
;; ==> nil
(balanced-mobile? m5)
;; ==> nil

;;
;; Let's construct a compound mobile, which is balanced:
;;



