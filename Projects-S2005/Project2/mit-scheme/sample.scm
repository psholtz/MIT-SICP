
(define (extract-entry play *game*)
  (define (compare play entry)
    (let ((test (car entry)))
      (and (string=? (car play) (car test))
	   (string=? (cadr play) (cadr test)))))
  (let
      ((first (list-ref *game* 0))
       (second (list-ref *game* 1))
       (third (list-ref *game* 2))
       (fourth (list-ref *game* 3)))

    (cond
     ((compare play first) first)
     ((compare play second) second)
     ((compare play third) third)
     ((compare play fourth) fourth)
     (else
      '()))))