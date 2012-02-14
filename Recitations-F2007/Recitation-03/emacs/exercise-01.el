;;
;; Exercise 1
;;

;; 
;; Emacs does not handle I/O exactly the same way that 
;; Scheme does. Accordingly, we will not be able to replicate
;; this exercise with exactly the same result as Scheme generates.
;;
;; For instance, emacs does not really have a "display" procedure
;; which functions the way that the corresponding procedure in 
;; Scheme does (i.e., a procedure which displays the argument and 
;; returns nothing). Therefore, we will "hack" a version of "display"
;; for Emacs that performs rather like the "display" method of Scheme:
;;
(defun display (x)
  (princ x)
  '())


;;
;; Consider the following definitions:
;;
(defun our-display (x)
  (display x)
  x)

;; ^^ Note that this definition of "our-display" functions very 
;; similar to the way in which the native "print", "princ", "prin1"
;; procedures in Emacs function.
(defun count1 (x)
  (cond ((= x 0) 0)
	(t
	 (our-display x)
	 (count1 (- x 1)))))

(defun count2 (x)
  (cond ((= x 0) 0)
	(t
	 (count2 (- x 1))
	 (our-display x))))

;;
;; What will "(count1 4)" and "(count2 4)" display?
;;

;;
;; These two procedures differ in where they apply the display to console, versus
;; where they apply the recursion.
;;
;; It is useful to work out the call graph for each invocation.
;;
;; Let's start with the call "(count1 4)":
;;

---------------------
(count1 )
--------------------- 
(our-display 4)
(count1 3)
--------------------- 

;; 
;; Let's think about what happens here:
;;
;; First we invoke (our-display 4). This procedure will print "4" to the console,
;; and then return the value 4. After the call to (our-display 4) returns, we 
;; next invoke (count1 3)
;;

;; 
;; What will be displayed on console will be "4", and whatever else will be displayed
;; and returned through a call to (count1 3).
;;

;; Our console thus looks like:
---------------------- 
4
--> (count1 3)
---------------------- 
;;
;; Where "-->" signifies an ongoing function evaluation. 
;;

;; 
;; Let's continue expanding (count1 3), and keep reference of what the console looks like:
;;
----------------------
4
--> (our-display 3)
--> (count1 2)
---------------------- 
43
--> (count1 2)
---------------------- 
43
--> (our-display 2)
--> (count1 1)
---------------------- 
432
--> (count1 1)
---------------------- 
432
--> (our-display 1)
--> (count1 0)
---------------------- 
4321
--> (count1 0)
---------------------- 
43210

;;
;; Since in our emacs implemented, the returned value ("0") will be rendered
;; on the same line as the rest of the output.
;;

;;
;; The procedure terminates and returns "0".
;;

;;
;; Now let's consider evaluation of (count2 4). Using the same notation as in the 
;; previous example, we have:
;;

----------------------
(count2 4)
---------------------- 
--> (count2 3)
--> (our-display 4)
---------------------- 
--> (count2 2)
--> (our-display 3)
--> (our-display 4)
---------------------- 
--> (count2 1)
--> (our-display 2)
--> (our-display 3)
--> (our-display 4)
---------------------- 
--> (count2 0)
--> (our-display 1)
--> (our-display 2)
--> (our-display 3)
--> (our-display 4)
---------------------- 

;;
;; The call to "(count2 0)" bottoms out and returns 0. 
;;
;; Thereafter we have four sequential calls to the "our-display" procedure.
;;
;; (our-display 1) returns 1, and then we invoke (our-display 2)
;; (our-display 2) returns 2, and then we invoke (our-display 3)
;; (our-display 3) returns 3, and then we invoke (our-display 4)
;; (our-display 4) returns 4.
;;
;; Hence, the overall invocation will return 4.
;;
-----------------------
12344
;; ==> 4
-----------------------