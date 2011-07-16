(load "prisoner.scm")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 1
;; 
;; Definition of "extract-entry"
;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 
(define (extract-entry play *game-association-list*)
  (define (compare play entry)
    (and (string=? (car play) (car (car entry)))
	 (string=? (cadr play) (cadr (car entry)))))
  (let
      ((first (list-ref *game-association-list* 0))
       (second (list-ref *game-association-list* 1))
       (third (list-ref *game-association-list* 2))
       (fourth (list-ref *game-association-list* 3)))
    (cond ((compare play first) first)
	  ((compare play second) second)
	  ((compare play third) third)
	  ((compare play fourth) fourth)
	  (else '()))))


