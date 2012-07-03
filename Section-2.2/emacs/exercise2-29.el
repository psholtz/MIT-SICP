
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