;;
;; 3.12

;; [[ WORKING ]]

;;
;; (define (append x y)
;;  (if (null? x)
;;      y
;;      (cons (car x) (append (cdr x) y))))
;;
;; Append forms a new list by successively "cons"-ing the elements of x onto y.
;; The procedure "append!" is similar to "append", but it is a mutator rather 
;; than a constructor. It appends the lists by splicing them together, modifying
;; the final pair of x so that its cdr is now y. (It is an error to call "append!"
;; with an empty x).
;;
;; (define (append! x y)
;;  (set-cdr! (last-pair x) y)
;;  x)
;;
;; Here "last-pair" is a procedure that retursn the last pair in its argument:
;;
;; (define (last-pair x)
;;  (if (null? (cdr x)
;;      x
;;      (last-pair? (cdr x))))
;;
;; Consider the interaction:
;;