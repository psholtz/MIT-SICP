
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