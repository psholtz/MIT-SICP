;;
;; Exercise 3.12
;; 
;; The following procedure for appending lists was introduced in section 2.2.1:
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
;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))
;; z
;; ==> (a b c d)
;; (cdr x)
;; <response>
;; (define w (append! x y))
;; w
;; ==> (a b c d)
;; (cdr x)
;; <response>
;;
;; What are the missing <response>s? Draw box-and-pointer diagrams to explain your answer.
;;
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
;; ==> (a b c d)
(cdr x)
;; ==> (b)

(define w (append! x y))
w
;; ==> (a b c d)
(cdr x)
;; ==> (b c d)

;;
;; Box diagram representing (define x (list 'a 'b)):
;;
       --- ---       --- --- 
x --> | * | * | --> | * | / | 
       --- ---       --- --- 
        |             | 
        v             v
       ---           --- 
      | a |         | b | 
       ---           ---
;;
;; Box diagram representing (define y (list 'c 'd)):
;;
       --- ---       --- ---
y --> | * | * | --> | * | / |
       --- ---       --- --- 
        |             |
        v             v
       ---           --- 
      | c |         | d | 
       ---           --- 
;;
;; Box diagram representing (define z (append x y))
;;

       --- ---       --- ---     y -->  --- ---       --- --- 
x --> | * | * | --> | * | / |          | * | * | --> | * | / |
       --- ---       --- ---      --->  --- ---       --- --- 
        |             |          |       |             | 
        v             v          |       v             v 
       ---           ---         |      ---           --- 
      | a |         | b |        |     | c |         | d | 
       ---           ---         |      ---           --- 
        ^             ^          | 
        |             |          |
       --- ---       --- ---     |
z --> | * | * | --> | * | * | --- 
       --- ---       --- --- 
      
;;
;; Clearly, at this point, invoking "z" will generate the list structure
;; to which "z" points (i.e., (list a b c d), and invoking "(cdr x)" will 
;; return the one element list to which the "cdr" of x points (i.e., (b))
;;

;;
;; After evaluating (append! x y), the corresponding box diagram will 
;; look like:
;;

                                    y
                                    |
                                    v
x -->  --- ---       --- ---       --- ---       --- --- 
      | * | * | --> | * | * | --> | * | * | --> | * | / |
w -->  --- ---       --- ---       --- ---       --- --- 
        |             |             |             |
        v             v             v             v
       ---           ---           ---           --- 
      | a |         | b |         | c |         | d | 
       ---           ---           ---           --- 

;;
;; "x" and "w" now both point to the same location in memory, and evaluating
;; "(cdr x)" will generate the same result as evaluating "(cdr w)", namely
;; (b c d).
;;