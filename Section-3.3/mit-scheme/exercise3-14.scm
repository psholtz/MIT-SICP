;;
;; Exercise 3.14
;;

;;
;; Define the "mystery" procedure:
;;
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;;
;; If we type this procedure into the interpreter, and evaluate expressions 
;; using it, we quickly see that all it does is "reverse" the input list structure.
;; Note that "mystery" only procedures a "shallow" reverse of the list data 
;; structures (i.e., it does not reverse embedded lists, within a top-level list).
;;
(mystery (list 'a 'b 'c 'd))
;; ==> (d c b a)
(mystery (list 'a 'b (list 'c 'd) 'e 'f))
;; ==> (f e (c d) b a)

;;
;; Let's step through the mystery procedure, and look specifically at how the "loop"
;; procedure is invoked. Suppose we invoke the inner "loop" procedure with two list
;; structures like (list 1 2 3 4) and (list 5 6 7 8):
;;
(loop (list 1 2 3 4) (list 5 6 7 8))

;;
;; The "temp" variable is assign (cdr x), i.e., (list 2 3 4):
;; temp <-- (list 2 3 4)
;;
;; and "set-cdr!" is invoked using "x" and "y":
;; x <-- (list 1 5 6 7 8)
;;
;; "loop" is then recursively invoked using "temp" and "x" as its arguments:
;; (loop (list 2 3 4) (list 1 5 6 7 8))
;;

;;
;; If we supply the "mystery" procedure with a list structure (list 'a 'b 'c 'd), 
;; the series of "loop" invocations will look like the following:
;;
(mystery (list 'a 'b 'c 'd))
(loop (list 'a 'b 'c 'd) '())
(loop (list 'b 'c 'd) (list 'a))
(loop (list 'c 'd) (list 'b 'a))
(loop (list 'd) (list 'c 'b 'a))
(loop '() (list 'd 'c 'b 'a))
;; ==> (d c b a)

;;
;; Define the list v:
;;
(define v (list 'a 'b 'c 'd))

;;
;; Draw box-and-pointer diagram illustrating the v list-structure:
;;
       --- ---       --- ---       --- ---       --- ---
v --> | * | * | --> | * | * | --> | * | * | --> | * | / |
       --- ---       --- ---       --- ---       --- --- 
        |             |             |             | 
        v             v             v             v
       ---           ---           ---           ---
      | a |         | b |         | c |         | d |  
       ---           ---           ---           --- 

;;
;; Suppose that we now evaluate (define w (mystery v)). Draw box-and-pointer
;; diagrams that show the structures v and w after evaluating this expression.
;; 
;; After one invocation, the variables look like:
;;
       --- ---              --- ---       --- ---       --- --- 
x --> | * | / |   temp --> | * | * | --> | * | * | --> | * | / |
       --- ---              --- ---       --- ---       --- --- 
        |                    |             |             |
        v                    v             v             v
       ---                  ---           ---           --- 
      | a |                | b |         | c |         | d |
       ---                  ---           ---           --- 
;;
;; After two invocations, the variables look like:
;;
       --- ---       --- ---              --- ---       --- --- 
x --> | * | * | --> | * | / |   temp --> | * | * | --> | * | / |
       --- ---       --- ---              --- ---       --- --- 
        |             |                    |             |
        v             v                    v             v
       ---           ---                  ---           ---
      | b |         | a |                | c |         | d | 
       ---           ---                  ---           --- 
;;
;; After three invocations, the variables look like:
;;
       --- ---       --- ---       --- ---              --- --- 
x --> | * | * | --> | * | * | --> | * | / |   temp --> | * | / |
       --- ---       --- ---       --- ---              --- ---
        |             |             |                    |
        v             v             v                    v
       ---           ---           ---                  ---
      | c |         | b |         | a |                | d | 
       ---           ---           ---                  --- 
;;
;; After four invocations, the variables look like:
;;
       --- ---       --- ---       --- ---       --- ---              --- 
x --> | * | * | --> | * | * | --> | * | * | --> | * | / |   temp --> | / |  
       --- ---       --- ---       --- ---       --- ---              --- 
        |             |             |             |
        v             v             v             v
       ---           ---           ---           ---
      | d |         | c |         | b |         | a | 
       ---           ---           ---           --- 
;;
;; The structure returned by the procedure is this structure "x", so we can write:
;;
       --- ---       --- ---       --- ---       --- --- 
w --> | * | * | --> | * | * | --> | * | * | --> | * | / |
       --- ---       --- ---       --- ---       --- --- 
        |             |             |             |
        v             v             v             v
       ---           ---           ---           ---
      | d |         | c |         | b |         | a | 
       ---           ---           ---           ---
