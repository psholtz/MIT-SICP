;;
;; Exercise 2.36
;;
;; The procedure "accumulate-n" is similar to accumulate except that it takes as its third
;; argument a sequence of sequences, which are all assumed to have the same number of elements.
;; It applies the designated accumulate procedure to combine all the first elements of the 
;; sequences, all the second elements of the sequences, and so on, and returns a sequence
;; of the results. For instance, if "s" is a sequence containing four sequences, 
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the
;; sequence (22 26 30). Fill in the missing expressionsin the following definition of 
;; "accumulate-n":
;;

;;
;; We need lexical bindings:
;;
(require 'cl)

;;
;; First define the "accumulate" procedure:
;;
(defun accumulate (op initial sequence)
  (lexical-let ((ops op))
    (if (null sequence)
	initial
      (funcall ops (car sequence)
	       (accumulate ops initial (cdr sequence))))))

;;
;; Run some unit tests:
;;
(accumulate #'+ 0 (list 1 2 3 4 5))
;; ==> 15
(accumulate #'* 1 (list 1 2 3 4 5))
;; ==> 120
(accumulate #'cons '() (list 1 2 3 4 5))
;; ==> (1 2 3 4 5)

;;
;; Define the "accumulate-n" procedure:
;;
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      '()
    (cons (accumulate op init (mapcar #'car seqs))
	  (accumulate-n op init (mapcar #'cdr seqs)))))

;;
;; Run some unit tests:
;;
(setq s (list
	 (list 1 2 3)
	 (list 4 5 6)
	 (list 7 8 9)
	 (list 10 11 12)))

(accumulate-n #'+ 0 s)
;; ==> (22 26 30)