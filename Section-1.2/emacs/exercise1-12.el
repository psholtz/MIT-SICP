;;
;; Exercise 1.12
;;
;; The following pattern of numbers is called Pascal's triangle.
;;
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1 
;;
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it. Write
;; a procedure that computes elements of Pascal's triangle by means
;; of a recursive procedure.
;;

;; 
;; Emacs lisp implementation follows. 
;;
;; See reference Scheme implementation for further details.
;;

(defun pascal (row col)
  (cond ((= col 1) 1)
	((= col row) 1)
	(t 
	 (+ 
	  (pascal (- row 1) (- col 1))
	  (pascal (- row 1) col)))))

;;
;; Run some simple unit tests.
;;
(= (pascal 1 1) 1)
(= (pascal 2 1) 1)
(= (pascal 2 2) 1)
(= (pascal 3 1) 1)
(= (pascal 3 2) 2)
(= (pascal 3 3) 1)
(= (pascal 4 1) 1)
(= (pascal 4 2) 3)
(= (pascal 4 3) 3)
(= (pascal 4 4) 1)
(= (pascal 5 1) 1)
(= (pascal 5 2) 4)
(= (pascal 5 3) 6)
(= (pascal 5 4) 4)
(= (pascal 5 5) 1)