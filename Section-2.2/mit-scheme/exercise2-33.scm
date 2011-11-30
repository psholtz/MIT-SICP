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

;;
;; (c) Next define the custom version of "length":
;;
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;
;; Run some unit tests:
;; 
(length '())
;; ==> 0
(length (list 112))
;; ==> 1
(length (list 33 22))
;; ==> 2
(length (list 33 22 111))
;; ==> 3
(length (list 'a 'b 'c))
;; ==> 3
(length (list 33 55 'a 'b 9000))
;; ==> 5