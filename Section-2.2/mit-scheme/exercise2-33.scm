;;
;; Exercise 2.33
;;
;; Fill in the missing expressions to complete the following definitions of some basic
;; list-manipulation operations as accumulations:
;;
;; "map"
;; "append"
;; "length"
;;

;;
;; First define "accumulate":
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;
;; (a) Next define the custom version of "map":
;;
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      '()
	      sequence))

;;
;; Run some unit tests:
;;
(map square (list 1 2 3 4 5))
;; ==> (1 4 9 16 25)

;;
;; (b) Next define the custom version of "append":
;;
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;;
;; Run some unit tests:
;;
(append (list 1 2 3) (list 4 5 6))
;; ==> (1 2 3 4 5 6)

[[ WORKING NOT DONE]]